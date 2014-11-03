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

;;  Resource metaclass

(defmethod closer-mop:finalize-inheritance :after ((rc resource-class))
  (setf (slot-value rc 'probes)
	(compute-probes rc)))

;;  Resource

(defmethod print-object ((r resource) stream)
  (print-unreadable-object (r stream :type t :identity (not *print-pretty*))
    (format stream "~S ~D spec ~D probed" (resource-id r)
	    (/ (length (specified-properties r)) 2)
	    (/ (length (probed-properties r)) 2))))

(defun make-resource (type id &rest initargs &key &allow-other-keys)
  (apply #'make-instance type :id id initargs))

(defun resource-type (resource)
  (class-name (class-of resource)))

;;  Resource tree

(defmethod print-object ((r resource-tree) stream)
  (print-unreadable-object (r stream :type t :identity (not *print-pretty*))
    (format stream "~S ~D spec ~D probed ~D nested" (resource-id r)
	    (/ (length (specified-properties r)) 2)
	    (/ (length (probed-properties r)) 2)
	    (hash-table-count (resources-of r)))))

;;  High level API

(defmacro get-resource (type id &optional (resources '*resources*))
  `(gethash (cons ,type ,id) ,resources))

(defun add-resource (parent child)
  (setf (get-resource (resource-type child) (resource-id child)
		      (resources-of parent))
	child))
