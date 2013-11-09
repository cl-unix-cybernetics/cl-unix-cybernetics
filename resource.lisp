;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2013 Thomas de Grivel <billitch@gmail.com>
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

;;  Resource class

(defclass resource-class (standard-class)
  ((instances :type hash-table
	      :initform (make-hash-table :test 'equal)
	      :reader resource-class-instances)))

(defmethod resource-class-instances ((class-name symbol))
  (resource-class-instances (find-class class-name)))

(defmethod closer-mop:validate-superclass ((class resource-class)
					   (super standard-class))
  t)

(defun resource-class-slot (name &rest rest &key initarg &allow-other-keys)
  (list* name
	 :initarg (or initarg
		      (intern (symbol-name name) :keyword))
	 rest))

(defmacro define-resource-class (name direct-superclasses
				 direct-slots
				 &optional options)
  `(defclass ,name ,(or direct-superclasses
			'(resource))
     ,(mapcar (lambda (x) (apply #'resource-class-slot x))
	      direct-slots)
     (:metaclass resource-class ,@options)))

;;  Resource

(defclass resource (standard-object)
  ((name :type t
	 :initform (error "missing resource name")
	 :initarg name
	 :reader resource-name))
  (:metaclass resource-class))

(defun make-resource (type name &rest properties)
  (apply #'make-instance type 'name name properties))

;;  Resource registration

(defgeneric register-resource (resource))

(defmethod register-resource ((res resource))
  "FIXME: lock instances index"
  (let ((name (resource-name res))
	(index (resource-class-instances (class-of res))))
    (assert (not (gethash name index)))
    (setf (gethash name index) res)))

(defun define-resource (type name &rest properties)
  (let ((res (apply #'make-resource type name properties)))
    (register-resource res)
    res))

(defgeneric find-resource (type name))

(defmethod find-resource ((type class) (name string))
  (gethash name (resource-class-instances type)))

(defmethod find-resource ((type symbol) name)
  (find-resource (find-class type) name))

;;  Resource property

(defun resource-property-slot-definition (resource property)
  (declare (type resource resource)
	   (type symbol property))
  (find-if (lambda (slot)
	     (let ((key (car (closer-mop:slot-definition-initargs slot))))
	       (eq property key)))
	   (closer-mop:class-slots (class-of resource))))

(defun resource-property-slot-name (resource property)
  (closer-mop:slot-definition-name
   (resource-property-slot-definition resource property)))

(defgeneric resource-property (resource property))

(defmethod resource-property ((resource resource)
			      (property symbol))
  (slot-value resource (resource-property-slot-name resource property)))

(defgeneric (setf resource-property) (new-value resource property))

(defmethod (setf resource-property) (new-value
				     (resource resource)
				     (property symbol))
  (setf (slot-value resource (resource-property-slot-name resource property))
	new-value))

;;  Resource properties

(defgeneric resource-properties (resource))

(defmethod resource-properties ((class resource-class))
  (iter (for slot in (closer-mop:class-slots class))
        (for key = (car (closer-mop:slot-definition-initargs slot)))
        (when (keywordp key)
	  (collect key))))

(defmethod resource-properties ((res resource))
  (resource-properties (class-of res)))

(defun mapcan-resource-properties (fn resource)
  (mapcan (lambda (slot)
	    (let ((key (car (closer-mop:slot-definition-initargs slot)))
		  (name (closer-mop:slot-definition-name slot)))
	      (when (and (slot-boundp resource name)
			 (keywordp key))
		(funcall fn key (slot-value resource name)))))
	  (closer-mop:class-slots (class-of resource))))

;;  Resource printing

(defun serialize-resource (resource)
  (let ((resource-class (class-of resource)))
    (list* 'make-instance
	   `',(class-name resource-class)
	   (resource-name resource)
	   (mapcan-resource-properties #'list resource))))

(defgeneric print-resource-property (resource property value stream))

(defmethod print-resource-property (resource property value stream)
  (format stream "~S" value))

(defmethod print-object ((res resource) s)
  (cond
    (*print-readably*
     (format s "(~W '~W ~W" 'make-instance (class-name (class-of res))
	     (resource-name res))
     #1=(pprint-logical-block (s (mapcan-resource-properties #'list res))
	  (iter (initially (pprint-exit-if-list-exhausted))
		(for property = (pprint-pop))
		(pprint-newline :fill s)
		(write-char #\Space s)
		(write property :stream s)
		(write-char #\Space s)
		(print-resource-property res property (pprint-pop) s)
		(pprint-exit-if-list-exhausted))))
    (:otherwise
     (print-unreadable-object (res s :type t :identity t)
       (write (resource-name res) :stream s)
       #1#
       (write-char #\Space s)))))

;;  Gathering resource values

(defgeneric gather-resource-property (resource property))

(defgeneric gather-resource (type name))

(defmethod gather-resource ((resource resource) (name t))
  (dolist (property (resource-properties resource))
    (setf (slot-value resource property)
	  (gather-resource-property resource property)))
  resource)

(defmethod gather-resource ((type class) (name string))
  (gather-resource (make-resource type name) name))

(defmethod gather-resource ((type symbol) name)
  (when (keywordp type)
    (setq type (find-symbol (symbol-name type) :adams)))
  (gather-resource (find-class type) name))

;;  Ensuring resource values

(defgeneric ensure-resource-property (resource property value))
