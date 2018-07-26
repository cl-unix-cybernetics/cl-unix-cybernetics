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

(in-package :common-lisp-user)

(defgeneric collect-sources (x))

(defmethod collect-sources ((x symbol))
  (collect-sources (asdf:find-system x)))

(defmethod collect-sources ((x string))
  (collect-sources (asdf:find-system x)))

(defun sort-components (list)
  (declare (type list list))
  (let (components roots)
    (declare (type list components roots))
    (labels ((map-dependencies (comp fn)
               (declare (type asdf:component comp))
               (dolist (id (asdf:component-sideway-dependencies comp))
                 (let ((dep (find id list :test #'string=
                                  :key #'asdf:component-name)))
                   (when dep
                     (funcall (the function fn) dep)))))
             (dfs (comp)
               (declare (type asdf:component comp))
               (map-dependencies comp #'dfs)
               (pushnew comp components)))
      (dolist (comp list)
        (declare (type asdf:component comp))
        (pushnew comp roots))
      (dolist (comp list)
        (declare (type asdf:component comp))
        (map-dependencies comp (lambda (dep)
                                 (setf roots (delete dep roots)))))
      (dolist (comp roots)
        (dfs comp)))
    (nreverse components)))

(defmethod collect-sources ((x asdf:parent-component))
  (let ((children (sort-components (asdf:component-children x))))
    (mapcan #'collect-sources children)))

(defmethod collect-sources ((req asdf:require-system))
  (list `(require ,(string-upcase (asdf:component-name req)))))

(defmethod collect-sources ((x asdf:cl-source-file))
  (list `(compile-lisp ,(asdf:component-pathname x))))

(defmethod collect-sources ((x asdf:file-component))
  (list `(quote ,(asdf:component-pathname x))))

(defmethod collect-sources :around ((x asdf:component))
  (let ((if-feature (asdf::component-if-feature x)))
    (if if-feature
        (when (find (the symbol if-feature) *features*)
          (call-next-method))
        (call-next-method))))

#+nil (collect-sources :adams)

(defvar *system-directory*
  (make-hash-table))

(defun system-directory (system)
  (or #1=(gethash system *system-directory*)
      (let* ((sys (typecase system (asdf:system system)
                            (t (asdf:find-system system))))
             (asd (asdf:system-source-file sys)))
        (setf #1#
              (make-pathname :name nil :type nil :defaults asd)))))

(defun system-file (system &rest parts)
  (let ((str (apply #'concatenate 'string parts)))
    (merge-pathnames str (system-directory system))))

(defun namestring* (x)
  (etypecase x
    (null "")
    (pathname (namestring x))
    (string x)))

(defparameter *dir* (namestring* (system-file :adams "")))

(defun compile-lisp (path)
  (let* ((adams-dir *dir*)
         (dir (pathname-directory path))
         (name (pathname-name path))
         (fasl (with-output-to-string (out)
                 (write-string adams-dir out)
                 (write-string "build/" out)
                 (dolist (d (rest dir))
                   (write-string d out)
                   (write-char #\- out))
                 (write-string name out)
                 (write-string ".fasl" out))))
      (print fasl)
      (unless (and (probe-file fasl)
                   (<= (file-write-date path)
                       (file-write-date fasl)))
        (print path)
        (ensure-directories-exist fasl)
        (compile-file path :output-file fasl))
      (load fasl)))

(defun write-system-build-file (system sbf)
  (format t "~&~A~%" sbf) (force-output)
  (with-open-file (out sbf :direction :output
                       :element-type 'character
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :external-format :utf-8)
    (declare (type stream out))
    (format out "~&;;  ~A" (asdf:component-name system))
    (dolist (src (collect-sources system))
      (print src out))))

(defun system-build-file (system)
  (let* ((adams-dir *dir*)
         (asd (asdf:system-source-file system))
         (name (substitute #\- #\/ (asdf:component-name system)))
         (sbf (concatenate 'string adams-dir "build/" name ".lisp")))
    (unless (and (probe-file sbf)
                 (<= (file-write-date asd)
                     (file-write-date sbf)))
      (write-system-build-file system sbf))
    sbf))

(defun system-and-dependencies (name)
  (let (dependencies)
    (labels ((dfs (name)
               (let ((sys (asdf:find-system name)))
                 (when sys
                   (locally (declare (type asdf:system sys))
                     (map 'nil #'dfs (asdf:system-depends-on sys))
                     (pushnew sys dependencies))))))
      (dfs name)
      (nreverse dependencies))))

(defun write-build-systems-file (system)
  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system system)))
  (let* ((path (system-file system "build/systems.lisp")))
    (print path) (force-output)
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                         :element-type 'character
                         :external-format :utf-8
                         :if-exists :overwrite
                         :if-does-not-exist :create)
      (declare (type stream out))
      (dolist (sys (system-and-dependencies system))
        (let* ((build-file (system-build-file sys))
               (load-form `(load ,build-file)))
          (format t "~& ~A~%" sys) (force-output)
          (print load-form out)))
      (fresh-line out))))

(write-build-systems-file :adams)

(defun load* (path)
  (format t "~&Loading ~S" path)
  (load path))

(load* (system-file :adams "config.lisp"))
(load* (system-file :adams "build/systems.lisp"))
(load* (system-file :adams "toplevel.lisp"))
