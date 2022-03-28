;;
;;  adams - system administrator written in Common Lisp
;;
;;  Copyright 2013,2014,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :adams)

(defvar *default-shell-command* "/bin/sh")
(defparameter *shell-signal-errors* nil)

(setf (debug-p :shell) t)

;;  String functions

(defun read-string (stream)
  (with-output-to-string (out)
    (loop for c = (when (listen stream)
		    (read-char stream))
       while c
       do (write-char c out))))

(defun make-random-bytes (length)
  (let ((seq (make-array length :element-type '(unsigned-byte 8))))
    (with-open-file (r #P"/dev/random" :element-type '(unsigned-byte 8))
      (read-sequence seq r))
    seq))

(defun make-random-string (length)
  (subseq (cl-base64:usb8-array-to-base64-string (make-random-bytes
						  (ceiling length 4/3))
						 :uri t)
	  0 length))

(defun debug-out (fmt &rest args)
  (let ((out *debug-io*))
    (apply #'format out fmt args)
    (force-output out)))

;;  Errors

(define-condition shell-error (error)
  ((command :type string
	    :initarg :command
	    :reader shell-error-command)
   (status :type fixnum
	   :initarg :status
	   :reader shell-error-status)
   (out :initarg :out
	:reader shell-error-out)
   (err :initarg :err
	:reader shell-error-err))
  (:report (lambda (e stream)
	     (with-slots (command status out err) e
	       (format stream "Shell command returned ~D
Command: ~S
Output: ~S
Error: ~S"
		       status command out err)))))

;;  Shell

(defun sh-quote (&rest str)
  (let ((str (str str)))
    (declare (type string str))
    (if (and (not (position #\Newline str))
             (cl-ppcre:scan "^[-+/=.,:^_0-9A-Za-z]*$" str))
        str
        (str #\" (re-subst "([$`\\\\\"])" "\\\\\\1" str) #\"))))

(defun sh-parse-integer (string)
  (when (< 0 (length string))
    (parse-integer string :radix (cond ((char= #\0 (char string 0)) 8)
				       (:otherwise 10)))))

(defun ascii-set-graphics-mode (stream &rest modes)
  (format stream "~C[~{~D~^;~}m" #\Esc modes))

(defun make-delimiter ()
  (format nil "---- ~A~A~A "
	  (ascii-set-graphics-mode nil 0 0 34 34 40 40)
	  (make-random-string 16)
	  (ascii-set-graphics-mode nil 0 0 37 37 40 40)))

(defclass shell ()
  ((command :type (or string list)
	    :initarg :command
	    :reader shell-command)
   (delimiter :type string
	      :reader shell-delimiter
	      :initform (make-delimiter))
   (log-stream :initarg :log-stream
	       :initform t
	       :reader shell-log-stream)))

(defgeneric shell-pid (shell))
(defgeneric shell-new-delimiter (shell))
(defgeneric shell-in (data shell))
(defgeneric shell-out/line (shell))
(defgeneric shell-err (shell))
(defgeneric shell-err/line (shell))
(defgeneric shell-status (shell))
(defgeneric shell-close (shell))
(defgeneric shell-closed-p (shell))
(defgeneric shell-run-command (command shell))
(defgeneric shell-log (shell fmt &rest args))
(defgeneric shell-log-p (shell))

(defmethod shell-log-p ((shell shell))
  (when (shell-log-stream shell)
    t))

(defmethod shell-log ((shell shell) (fmt string) &rest args)
  (let ((log (shell-log-stream shell)))
    (when log
      (format log "~&~D" (shell-pid shell))
      (apply #'format log fmt args)
      (force-output log))))

(defmethod shell-status ((shell shell))
  (let* ((delim (the string (make-delimiter)))
	 (len (length delim))
	 (lines-head (cons (str #\Newline) nil))
	 (lines-tail lines-head))
    (shell-in (format nil " ; echo \"~%~A $?\"~%" delim) shell)
    (let* ((prev nil)
           (status
            (loop (let ((line (shell-out/line shell)))
                    (when (or (null line)
                              (and (< len (length line))
                                   (string= delim line :end2 len)))
                      (unless (or (null prev) (string= "" prev))
                        (setf (cdr lines-tail) (cons prev nil)))
                      (return (when line
                                (when (debug-p :shell*)
                                  (debug-out "$ "))
                                (parse-integer line :start len))))
                    (when prev
                      (when (debug-p :shell*)
                        (debug-out "~A~%" prev))
                      (setf (cdr lines-tail) (cons (str prev #\Newline) nil)
                            lines-tail (cdr lines-tail)))
                    (setf prev line))))
	   (out (cdr lines-head))
	   (err (shell-err/line shell)))
      (when (shell-log-p shell)
	(dolist (line out)
	  (shell-log shell "| ~A~%" line))
	(dolist (line err)
	  (shell-log shell "# ~A~&" line))
        (unless (and status (= 0 status))
          (shell-log shell   " ⇒ ~D~%" status)))
      (values status out err))))

;;  Run command

(defmethod shell-run-command ((command string) (shell shell))
  (when (debug-p :shell)
    (format t "~&~D╭ $ ~A~%" (shell-pid shell) command))
  (shell-in command shell)
  (shell-status shell))

;;  High level interface

(defmacro with-shell ((shell &optional (command *default-shell-command*))
		      &body body)
  (let ((g!shell (gensym "SHELL-")))
    `(let ((,g!shell (make-shell ,command)))
       (unwind-protect (let ((,shell ,g!shell)) ,@body)
	 (shell-close ,g!shell)))))

(defun shell-run (shell &rest command)
  (let ((cmd (str command)))
    (multiple-value-bind (status out err) (shell-run-command cmd shell)
      (when *shell-signal-errors*
	(assert (= 0 status) ()
		'shell-error :command cmd :status status :out out :err err))
      (values out status err))))
