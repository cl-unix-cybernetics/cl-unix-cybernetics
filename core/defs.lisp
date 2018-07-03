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

;;  Misc

(unless (boundp '+undefined+)
  (defconstant +undefined+ '+undefined+))

(defmethod subclasses (class))

;;  Properties

(defgeneric compare-property-values (resource property value1 value2))

(defgeneric merge-property-values (resource property old new))

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

;;  Operations

(defclass operation ()
  ((name :initarg :name
	 :initform (error "Operation without a name.")
	 :reader operation-name
	 :type symbol)
   (properties :initarg :properties
	       :initform (error "Operation without properties.")
	       :reader operation-properties)
   (after :initarg :after
          :reader operations-before)))

(defgeneric operation-generic-function (op))

;;  Resource metaclass

(defvar *the-resource-class*)

(defclass resource-class (standard-class)
  ((direct-probes :initarg :direct-probes
		  :initform ()
		  :reader direct-probes
		  :type list)
   (direct-operations :initarg :direct-operations
                      :initform ()
                      :reader direct-operations
                      :type list)
   (probes :reader probes-of
	   :type list)
   (operations :reader operations-of
               :type list))
  (:default-initargs :direct-superclasses (list *the-resource-class*)))

(defmethod closer-mop:validate-superclass ((c resource-class)
					   (super standard-class))
  t)

(defmacro define-resource-class (name &optional
                                        direct-superclasses
                                        direct-slots
                                        direct-probes
                                        direct-ops
                                        options)
  `(defclass ,name ,(or direct-superclasses
			'(resource))
     ,direct-slots
     (:direct-operations ,@direct-ops)
     (:direct-probes ,@direct-probes)
     (:metaclass resource-class)
     ,@options))

(defgeneric probe-class (resource-class))
(defgeneric compute-probes (resource-class))

(defgeneric operation-class (resource-class))
(defgeneric compute-operations (resource-class))

;;  Resource base class

(defclass resource (standard-object)
  ((id :type atom
       :initarg :id
       :initform (error "Missing ID for resource.")
       :reader resource-id)
   (specified-properties :type list
			 :initarg :specified-properties
			 :initform nil
			 :accessor specified-properties)
   (probed-properties :type list
		      :initarg :probed-properties
		      :initform nil
		      :accessor probed-properties))
  (:metaclass resource-class))

(defgeneric resource-probes-properties (resource))

(setq *the-resource-class* (find-class 'resource))

(defvar *resource*)

;;  Resource container

(defun make-resource-registry ()
  (make-hash-table :test 'equalp))

(defclass resource-container (resource)
  ((resources :initarg :resources
	      :initform (make-resource-registry)
	      :type hash-table
	      :reader resource-registry))
  (:metaclass resource-class))

(defvar *adams*
  (make-instance 'resource-container :id "Adams"))

(defvar *parent-resource*
  *adams*)

(defgeneric clear-resources% (resource-container))

(defmacro with-parent-resource (resource &body body)
  `(let ((*parent-resource* ,(or resource '*adams*)))
     ,@body))

(defgeneric resource-before-p (r1 r2))

;;  Specifying resources

(defgeneric specified-property (resource property))
(defgeneric (setf specified-property) (value resource property))

(defvar *specification*)
(defgeneric parse-next-specification (resource spec))
(defgeneric parse-specification (resource spec))

;;  OS

(defclass os ()
  ((machine :initarg :machine
	    :reader os-machine
	    :type string)
   (name :initarg :name
	 :reader os-name
	 :type (or symbol string))
   (release :initarg :release
	    :reader os-release
	    :type string)
   (version :initarg :version
	    :reader os-version
	    :type string)))

;;  Host

(define-resource-class host (resource-container)
  ((shell :initarg :shell
	  :type shell))
  ((probe-os-using-uname :properties (:os))
   (probe-hostname :properties (:hostname))
   (probe-boot-time :properties (:boot-time))
   (probe-host-user :properties (:user))
   (probe-host-packages :properties (:packages)))
  ((op-hostname :properties (:hostname))))

(defgeneric probe-os-using-uname (host os))
(defgeneric probe-hostname (host os))
(defgeneric probe-boot-time (host os))

(defgeneric host-disconnect (host))
(defgeneric host-shell (host))
(defgeneric (setf host-shell) (shell host))

(defgeneric host-os (host))
(defgeneric host-run (host &rest command))

(defvar *host*)

;;  Probing resources

(defgeneric find-probe (resource property os))
(defgeneric probe (resource property))
(defgeneric get-probed (resource property))
(defgeneric clear-probed% (resource properties))
(defgeneric describe-probed% (resource output))
(defgeneric describe-probed-property-value (resource property value))

(define-condition resource-probe-error (error)
  ((resource :initarg :resource)
   (property :initarg :property)
   (host :initarg :host
	 :initform (current-host))
   (os :initarg :os)))

(define-condition resource-probe-not-found (resource-probe-error)
  ())

(define-condition resource-probe-failed (resource-probe-error)
  ((probe :initarg :probe)))

;;  Operating on resources

(defgeneric find-operation (resource property os))
(defgeneric list-operations (resource plist os))
(defgeneric operate (resource plist))

(define-condition resource-operation-error (error)
  ((resource :initarg :resource)
   (host :initarg :host
	 :initform (current-host))
   (os :initarg :os)))

(define-condition resource-operation-not-found (resource-operation-error)
  ((property :initarg :property)))

(define-condition resource-operation-failed (resource-operation-error)
  ((operation :initarg :operation)
   (diff :initarg :diff)))

;;  Operators on property lists

(defmacro remf* (place indicator)
  `(loop
      (unless (remf ,place ,indicator)
        (return))))
