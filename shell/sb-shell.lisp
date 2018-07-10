;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2013,2014 Thomas de Grivel <thomas@lowh.net>
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

;;  SBCL implementation

(defclass sb-shell (shell)
  ((process :type sb-impl::process
	    :initarg :process
	    :reader shell-process)
   (command :type string
	    :initarg command
	    :reader shell-command)))

;;  Shell infos

(defmethod shell-close ((shell sb-shell))
  (format t "~&Closing shell : ~A~%" shell)
  (force-output)
  (sb-ext:process-kill (shell-process shell) sb-unix:sigterm)
  (sb-ext:process-close (shell-process shell)))

(defmethod shell-closed-p ((shell sb-shell))
  (not (eq :running (sb-ext:process-status (shell-process shell)))))

(defmethod shell-pid ((shell sb-shell))
  (sb-ext:process-pid (shell-process shell)))

(defmethod print-object ((shell sb-shell) stream)
  (print-unreadable-object (shell stream :type t)
    (format stream "~D ~S" (shell-pid shell) (shell-command shell))))

(defun make-shell (&optional (command *default-shell-command*) &rest args)
  (format t "~&Opening shell : ~A~{ ~A~}~%" command args)
  (force-output)
  (let ((process (sb-ext:run-program command args
				     :wait nil :search nil
				     :input :stream
				     :output :stream
				     :error :stream)))
    (unwind-protect
	 (let ((shell (make-instance 'sb-shell
				     :process process
				     :command (format nil "~A~{ ~A~}"
						      command args))))
	   (shell-in "true" shell)
	   (shell-status shell)
	   (setq process nil)
	   shell)
      (when process
	(sb-ext:process-kill process sb-unix:sigterm)
	(sb-ext:process-close process)))))

(defmethod shell-in ((data string)
		     (shell sb-shell))
  (let ((in (sb-ext:process-input (shell-process shell))))
    (write-string data in)
    (finish-output in)
    (when (debug-p :sb-shell)
      (debug-out "~A" data))
    shell))

(defmethod shell-out/line ((shell sb-shell))
  (let ((out (read-line (sb-ext:process-output (shell-process shell)) nil nil)))
    (when (and out (debug-p :sb-shell))
      (debug-out "~A~%" out))
    out))

(defmethod shell-err ((shell sb-shell))
 (let ((err (read-string (sb-ext:process-error (shell-process shell)))))
   (when (debug-p (or :sb-shell))
     (debug-out "~A" err))
   err))

(defmethod shell-err/line ((shell sb-shell))
  (cl-ppcre:split #.(make-string 1 :initial-element #\Newline)
		  (shell-err shell)))
