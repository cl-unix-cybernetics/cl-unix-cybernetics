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

;;  Resource container

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

;;  Sorting resources

(defmethod resource-before-p ((r1 resource) (r2 resource))
  nil)

;;  Sync

(defmethod sync :after ((res resource-container))
  (with-parent-resource res
    (let ((sorted-resources (sorted-resources res)))
      (loop
         (when (endp sorted-resources)
           (return))
         (let ((child (pop sorted-resources)))
           (sync child))))))
