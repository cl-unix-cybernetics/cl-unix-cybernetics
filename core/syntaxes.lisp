;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(in-re-readtable)

;;  Simple regexp-based parser generator with ITERATE support

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-vars (specs)
    (let ((vars))
      (dolist (spec specs)
        (if (consp spec)
            (dolist (var (rest spec))
              (push var vars))
            (push spec vars)))
      (nreverse vars))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-values (specs)
    (let ((values))
      (dolist (spec specs)
        (if (consp spec)
            (let ((fun (first spec)))
              (when (and (consp fun) (eq 'function (first fun)))
                (setq fun (second fun)))
              (dolist (var (rest spec))
                (push `(when ,var (,fun ,var)) values)))
            (push spec values)))
      (nreverse values))))

(defmacro define-syntax (name specs re &body body)
  (let* ((parse-name (sym 'parse- name))
         (with-name (sym 'with- name))
         (doc (when (stringp (first body)) (pop body)))
         (vars (collect-vars specs))
         (values (collect-values specs)))
    `(progn
       (defun ,parse-name (line)
	 ,@(when doc (list doc))
         (declare (type string line))
	 (re-bind ,re ,vars line
	   ,@(or body `((values ,@values)))))
       (defmacro ,with-name ((,@vars) lines &body with-body)
	 ,@(when doc (list doc))
         `(block nil
            (dolist (line ,lines)
              (declare (type string line))
              (multiple-value-bind (,,@vars) (,',parse-name line)
                (declare (ignorable ,,@vars))
                ,@with-body)))))))

;;  Host

(defun parse-uptime (string)
  (or (re-bind #~|^\s*([0-9]+ days?,\s*)?([0-9]+):([0-9]+)\s*$|
          (d h m) string
        (let* ((im (if m (parse-integer m) 0))
               (ih (if h (parse-integer h) 0))
               (id (if d (parse-integer d :junk-allowed t) 0))
               (id-hours (* 24 id))
               (hours (+ ih id-hours))
               (hours-minutes (* 60 hours))
               (minutes (+ im hours-minutes))
               (seconds (* 60 minutes)))
          (declare (type fixnum im ih id id-hours hours
                         hours-minutes minutes seconds))
          seconds))
      (error "Invalid uptime ?")))

(defun parse-comma-number (str)
  (declare (type string str))
  (dotimes (i (the fixnum (length str)))
    (when (char= #\, (char str i))
      (setf (char str i) #\.)))
  (parse-number str))

(define-syntax uptime<1> ((#'chronicity:parse time)
                          (#'parse-uptime uptime)
                          (#'parse-integer users)
                          (#'parse-comma-number load1 load5 load15))
  #~|^\s*(\S+)\s+up\s+(.+),\s+([0-9]+)\s+users?,\s+load averages?: ([0-9.,]+), ([0-9.,]+), ([0-9.,]+)$|
  "Syntax of the uptime command output. See uptime(1).")
