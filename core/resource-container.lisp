;; cl-unix-cybernetics
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(in-package :cl-unix-cybernetics)

;;  Resource registry

(defun resource-registry-count (rr)
  (hash-table-count rr))

(defun clear-resource-registry (rr)
  (clrhash rr))

(defun get-resource (type id &optional (container *parent-resource*))
  (gethash (cons type id)
           (typecase container
             (resource-container (resource-registry container))
             (t container))))

(defsetf get-resource (type id &optional (container '*parent-resource*))
    (value)
  `(setf (gethash (cons ,type ,id)
                  (typecase ,container
                    (resource-container (resource-registry ,container))
                    (t ,container)))
         ,value))

(defun add-resource (parent child)
  (setf (get-resource (resource-type child)
                      (resource-id child)
		      parent)
	child))

(defmacro do-resources% ((var) container &body body)
  (let ((x (gensym)))
    `(maphash (lambda (,x ,var)
                (declare (ignore ,x))
                ,@body)
              (resource-registry ,container))))

(defmethod sorted-resources ((res resource-container))
  (let ((resources))
    (do-resources% (child) res
      (push child resources))
    (sort resources #'resource-before-p)))

(defmacro do-resources ((var) container &body body)
  (let ((resources (gensym "RESOURCES-")))
    `(let ((,resources (sorted-resources ,container)))
       (loop (when (endp ,resources) (return))
          (let ((,var (pop ,resources)))
            (declare (type resource ,var))
            ,@body)))))

(defmethod print-object ((rc resource-container) stream)
  (print-unreadable-object (rc stream :type t :identity (not *print-pretty*))
    (format stream "~S ~D spec ~D probed ~D resources" (resource-id rc)
	    (/ (length (specified-properties rc)) 2)
	    (/ (length (probed-properties rc)) 2)
	    (resource-registry-count (resource-registry rc)))))

(defmethod clear-resources% ((rc resource-container))
  (clear-resource-registry (resource-registry rc)))

(defun clear-resources (&optional (resource-container *parent-resource*))
  (clear-resources% resource-container))

(defmethod resource-before-p ((r1 resource) (r2 resource))
  nil)

(defmethod resource-additional-specs ((res resource-container) (os t))
  (call-next-method)
  (with-parent-resource res
    (do-resources (child) res
      (resource-additional-specs child os))))

(defmethod sync :after ((res resource-container))
  (with-parent-resource res
    (let ((sorted-resources (sorted-resources res)))
      (loop
         (when (endp sorted-resources)
           (return))
         (let ((child (pop sorted-resources)))
           (sync child))))))
