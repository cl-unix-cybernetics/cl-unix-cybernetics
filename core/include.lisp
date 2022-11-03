;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(defun include/resolve-filename (spec)
  (flet ((try (&rest parts)
           (let ((path (str parts)))
             (when (probe-file path)
               (return-from include/resolve-filename path)))))
    (try spec ".adams")
    (try spec)))

(defun include/resolve-filename! (spec)
  (or (include/resolve-filename spec)
      (error "(include ~S) => file not found.~%
Current directory : ~S" spec *default-pathname-defaults*)))

(defun include (&rest sources)
  (let* ((head (cons 'list nil))
         (tail head)
         (eof (gensym "EOF")))
    (dolist (source sources)
      (let ((path (include/resolve-filename! source)))
        (with-open-file (in path
                            :element-type 'character
                            :external-format :utf-8)
          (loop
             (let ((form (read in nil eof)))
               (when (eq form eof)
                 (return))
               (setf (rest tail) (cons form nil)
                     tail (rest tail)))))))
    head))
