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

(unless (boundp '+undefined+)
  (defconstant +undefined+ '#:undefined))

;;  Probe

(defclass probe ()
  ((name :initarg :name
	 :initform (error "Probe without a name.")
	 :reader probe-name
	 :type symbol)
   (properties :initarg :properties
	       :initform (error "Probe without properties.")
	       :reader probe-properties)))

(defgeneric probe-generic-function (probe))

(defmethod probe-generic-function ((probe probe))
  (symbol-function (probe-name probe)))

(defmethod print-object ((probe probe) stream)
  (print-unreadable-object (probe stream :type t :identity (not *print-pretty*))
    (format stream "~S (~{~A~^ ~})"
	    (probe-name probe)
	    (probe-properties probe))))

;;  Resource meta class

(defvar *the-resource-class*)

(defclass resource-class (standard-class)
  ((direct-probes :initarg :direct-probes
		  :initform ()
		  :reader resource-class-direct-probes
		  :type list)
   (probes :initarg :direct-probes
	   :initform ()
	   :reader resource-class-probes
	   :type list))
  (:default-initargs :direct-superclasses (list *the-resource-class*)))

(defmethod closer-mop:validate-superclass ((class resource-class)
					   (super standard-class))
  t)

(defgeneric resource-class-probe-class (resource-class))

(defmethod resource-class-probe-class ((resource-class resource-class))
  'probe)

(defgeneric compute-probes (resource-class))

(defmethod compute-probes ((resource-class resource-class))
  (iter (for class in (closer-mop:class-precedence-list resource-class))
        (for direct-probes = (when (typep class 'resource-class)
			       (resource-class-direct-probes class)))
	(dolist (probe-definition direct-probes)
	  (collect (apply #'make-instance
			  (resource-class-probe-class resource-class)
			  :name probe-definition)))))

(defmethod closer-mop:finalize-inheritance :after ((resource-class resource-class))
  (setf (slot-value resource-class 'probes)
	(compute-probes resource-class)))

(defmacro define-resource-class (name direct-superclasses
				 direct-slots direct-probes
				 &optional options)
  `(defclass ,name ,(or direct-superclasses
			'(resource))
     ,direct-slots
     (:metaclass resource-class)
     (:direct-probes ,@direct-probes)
     ,@options))

;;  Resources

(defclass resource (standard-object)
  ((id :type atom
       :initarg :id
       :initform (error "Missing ID for resource.")
       :reader resource-id)
   (specified-properties :type list
			 :initarg :specified-properties
			 :initform nil
			 :reader specified-properties)
   (probed-properties :type list
		      :initarg :probed-properties
		      :initform nil
		      :reader probed-properties))
  (:metaclass resource-class))

(setq *the-resource-class* (find-class 'resource))

(defmethod print-object ((res resource) stream)
  (print-unreadable-object (res stream :type t :identity *print-readably*)
    (format stream "~S ~D ~D" (resource-id res)
	    (/ (length (specified-properties res)) 2)
	    (/ (length (probed-properties res)) 2))))

;;  Probes

(defun os-class (os)
  (etypecase os
    (null t)
    ((eql t) t)
    (symbol (find-class os))
    (os (class-of os))
    (standard-class os)))

(defgeneric find-probe (resource property os))

(defmethod find-probe ((resource resource)
		       (property symbol)
		       os)
    (some (lambda (probe)
	    (when (find property (probe-properties probe) :test #'eq)
	      (let ((f (probe-generic-function probe)))
		(when (compute-applicable-methods f (list resource os))
		  f))))
	  (resource-class-probes (class-of resource))))

(defgeneric probe (resource property))

(defmethod probe ((resource resource) (property symbol))
  (with-slots (probed-properties) resource
    (let* ((os (os))
	   (probe (or (find-probe resource property os)
		      (error "No probe found for ~S property ~S on ~A"
			     resource property (class-name (class-of os)))))
	   (result (funcall probe resource os)))
      (when (eq +undefined+ (getf result property +undefined+))
	(error "Probe did not return expected property.~%~
                resource: ~S~%~
                property: ~S~%~
                probe: ~S~%~
                result: ~S"
	       resource property probe result))
      (setf probed-properties
	    (append result probed-properties))
      result)))

(defgeneric get-probed (resource property))

(defmethod get-probed ((resource resource) (property symbol))
  (let ((value (getf (probed-properties resource) property +undefined+)))
    (when (eq +undefined+ value)
      (setq value (getf (probe resource property) property +undefined+)))
    (when (eq +undefined+ value)
      (error "Probe did not return expected property."))
    value))

(defun make-resource (type id &rest initargs &key &allow-other-keys)
  (apply #'make-instance type :id id initargs))
