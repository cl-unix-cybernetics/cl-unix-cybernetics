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

;;  Operations

(defclass operation ()
  ((name :initarg :name
	 :initform (error "Operation without a name.")
	 :reader op-name
	 :type symbol)
   (properties :initarg :properties
	       :initform (error "Operation without properties.")
	       :reader op-properties)))

;;  Resource metaclass

(defvar *the-resource-class*)

(defclass resource-class (standard-class)
  ((direct-probes :initarg :direct-probes
		  :initform ()
		  :reader direct-probes
		  :type list)
   (direct-ops :initarg :direct-ops
	       :initform ()
	       :reader direct-ops
	       :type list)
   (probes :reader probes-of
	   :type list)
   (ops :reader ops-of
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
     (:direct-ops ,@direct-ops)
     (:direct-probes ,@direct-probes)
     (:metaclass resource-class)
     ,@options))

(defgeneric probe-class (resource-class))
(defgeneric compute-probes (resource-class))

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

;;  Resource registry

(defun make-*resources* ()
  (make-hash-table :test 'equalp))

(defvar *resources*
  (make-*resources*))

;;  Resource tree class

(defclass resource-tree (resource)
  ((resources :initarg :resources
	      :initform (make-*resources*)
	      :type hashtable
	      :reader resources-of))
  (:metaclass resource-class))

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
	 :type string)
   (release :initarg :release
	    :reader os-release
	    :type string)
   (version :initarg :version
	    :reader os-version
	    :type string)))

;;  Host

(define-resource-class host (resource-tree)
  ((shell :initarg :shell
	  :type shell))
  ((probe-os-using-uname :properties (:os))
   (probe-hostname :properties (:hostname))
   (probe-uptime :properties (:uptime))))

(defgeneric host-connect (host))
(defgeneric host-disconnect (host))
(defgeneric host-shell (host))
(defgeneric (setf host-shell) (shell host))

(defgeneric host-run (host command &rest format-args))

(define-resource-class ssh-host (host))

(defvar *localhost*)
(defvar *host*)

;;  Probing resources

(define-condition resource-probe-error (error)
  ((resource :initarg :resource)
   (property :initarg :property)
   (host :initarg :host
	 :initform *host*)
   (os :initarg :os)))

(define-condition resource-probe-not-found (resource-probe-error)
  ())

(define-condition resource-probe-failed (resource-probe-error)
  ((probe :initarg :probe)))

(defgeneric find-probe (resource property os))
(defgeneric probe (resource property))
(defgeneric get-probed (resource property))

(defvar *resource*)
