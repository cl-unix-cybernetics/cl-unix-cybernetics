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

;;  Specified properties, what all specifications amount to.

(defmethod specified-property ((res resource)
			       (property symbol))
  (let ((value (getf (specified-properties res) property +undefined+)))
    (when (eq +undefined+ value)
      (error "Property ~S not specified for ~S." property res))
    value))

(defmethod (setf specified-property) (value
				      (res resource)
				      (property symbol))
  (setf (getf (specified-properties res) property) value))

;;  Parse specifications

(defmethod parse-next-specification ((res resource) spec)
  (let ((property (pop spec))
	(value (pop spec)))
    (setf (specified-property res property) value)
    spec))

(defmethod parse-next-specification ((res resource-tree) spec)
  (cond ((typep (first spec) 'resource)
	 (add-resource res (pop spec))
	 spec)
	(:otherwise (call-next-method))))

(defmethod parse-specification ((res resource) (spec cons))
  (iter (while spec)
	(for next-spec = (parse-next-specification res spec))
	(when (eq spec next-spec)
	  (error "Invalid specification : ~S" spec))
	(setq spec next-spec)))

(defmethod subclasses ((class class))
  (let (r)
    (labels ((walk (c)
	       (dolist (sub (sort (copy-seq
				   (closer-mop:class-direct-subclasses c))
				  #'string< :key #'class-name))
		 (pushnew sub r)
		 (walk sub))))
      (walk class))
    (nreverse r)))

(defun resource (type id &rest spec)
  (let ((r (or #1=(get-resource type id)
	       (setf #1# (make-resource type id)))))
    (when spec
      (parse-specification r spec))
    r))

(defmacro specify (&body specification)
  `(macrolet ,(mapcar (lambda (c)
			`(,(class-name c) (id &body s)
			   `(resource ',',(class-name c) ,id ,@s)))
		      (subclasses (find-class 'resource)))
     (parse-specification *localhost*
			  (list ,@specification))))
