
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
  (list `(load (compile-file ,(asdf:component-pathname x)))))

(defmethod collect-sources ((x asdf:file-component))
  (list `(quote ,(asdf:component-pathname x))))

(defmethod collect-sources :around ((x asdf:component))
  (let ((if-feature (asdf::component-if-feature x)))
    (if if-feature
        (when (find (the symbol if-feature) *features*)
          (call-next-method))
        (call-next-method))))

#+nil (collect-sources :adams)

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

(defun system-directory (system)
  (unless (typep system 'asdf:system)
    (setq system (asdf:find-system system)))
  (make-pathname :name nil
                 :type nil
                 :defaults (asdf:system-source-file system)))

(defun system-file (system &rest parts)
  (let ((str (apply #'concatenate 'string parts)))
    (merge-pathnames str (system-directory system))))

(defun build/systems.lisp (system)
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
        (let ((sys-name (asdf:component-name sys)))
          (format t "~& ~A~%" sys) (force-output)
          (format out "~&;;  ~A" sys-name))
        (dolist (src (collect-sources sys))
          (print src out))))))

(build/systems.lisp :adams)
(load (system-file :adams "build/systems.lisp"))
(load (system-file :adams "toplevel.lisp"))
