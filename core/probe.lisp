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

;;  Probe methods

(defmethod probe-generic-function ((probe probe))
  (symbol-function (probe-name probe)))

(defmethod print-object ((probe probe) stream)
  (print-unreadable-object (probe stream :type t :identity (not *print-pretty*))
    (format stream "~S (~{~A~^ ~})"
	    (probe-name probe)
	    (probe-properties probe))))

;;  Relate probes to properties in each resource class

(defmethod probe-class ((rc resource-class))
  'probe)

(defmethod compute-probes ((rc resource-class))
  (let ((class-precedence-list (closer-mop:class-precedence-list rc))
        (probes))
    (loop
       (when (endp class-precedence-list)
         (return))
       (let* ((class (pop class-precedence-list))
              (direct-probes (when (typep class 'resource-class)
                               (direct-probes class))))
         (dolist (probe-definition direct-probes)
           (let ((probe (apply #'make-instance (probe-class rc)
                               :name probe-definition)))
             (push probe probes)))))
    (nreverse probes)))

(defmethod probe-properties ((rc resource-class))
  (let ((properties nil))
    (dolist (probe (probes-of rc))
      (dolist (property (probe-properties probe))
	(pushnew property properties)))
    (sort properties #'string<)))

;;  Probing resources

(defmethod probe-properties ((r resource))
  (probe-properties (class-of r)))

(defmethod probes-of ((r resource))
  (probes-of (class-of r)))

(defmethod find-probe ((r resource)
		       (property symbol)
		       os)
    (some (lambda (probe)
	    (when (find property (probe-properties probe) :test #'eq)
	      (let ((f (probe-generic-function probe)))
		(when (compute-applicable-methods f (list r os))
		  probe))))
	  (probes-of r)))

(defun add-probed-properties (resource properties)
  (format t "~&(resource '~A ~S ~{~S ~S~^ ~})~%"
          (class-name (class-of resource))
          (resource-id resource)
          properties)
  (setf #1=(probed-properties resource)
        (merge-properties resource #1# properties)))

(defmethod probe ((r resource) (property symbol))
  (let* ((os (unless (and (typep r 'host)
                          (eq property :os))
               (host-os (current-host))))
	 (probe (or (find-probe r property os)
		    (error 'resource-probe-not-found
			   :resource r
			   :property property
			   :host (current-host)
			   :os os)))
	 (result (funcall (probe-generic-function probe) r os)))
    (when (eq +undefined+ (get-property property result))
      (error 'resource-probe-failed
	     :probe probe
	     :resource r
	     :property property
	     :host (current-host)
	     :os os))
    (add-probed-properties r result)
    result))

(defmethod probe ((host host) (property symbol))
  (with-host host
    (call-next-method)))

(defmethod get-probed ((r resource) (property symbol))
  (let ((value (get-property property (probed-properties r))))
    (when (eq +undefined+ value)
      (setq value (get-property property (probe r property))))
    value))

(defmethod clear-probed% ((r resource) (properties null))
  (setf (probed-properties r) nil))

(defmethod clear-probed% ((r resource) (property symbol))
  (remf* (probed-properties r) property))

(defmethod clear-probed% ((r resource) (properties cons))
  (dolist (p properties)
    (when p
      (clear-probed r p))))

(defmethod clear-probed% ((res resource-container) properties)
  (call-next-method)
  (do-resources (child) res
    (clear-probed% child properties)))

(defun clear-probed (&optional (resource *parent-resource*) properties)
  (clear-probed% resource properties))

;;  Conditions

(defmethod print-object ((c resource-probe-not-found) stream)
  (if *print-escape*
      (call-next-method)
      (with-slots (resource property host os) c
	(format stream "Probe not found~%resource ~A~%property ~A~%host ~S~%~A"
		resource property (hostname host)
		(class-name (class-of os))))))

(defmethod print-object ((c resource-probe-failed) stream)
  (if *print-escape*
      (call-next-method)
      (with-slots (probe resource property host os) c
	(format stream "~A failed~%resource ~A~%property ~A~%host ~S~%~A"
		(probe-name probe) resource property (hostname host)
		(class-name (class-of os))))))
