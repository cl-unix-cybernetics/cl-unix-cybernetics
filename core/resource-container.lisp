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
