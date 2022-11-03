;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

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
