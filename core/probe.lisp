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
  (iter (for class in (closer-mop:class-precedence-list rc))
        (for direct-probes = (when (typep class 'resource-class)
			       (direct-probes class)))
	(dolist (probe-definition direct-probes)
	  (collect (apply #'make-instance
			  (probe-class rc)
			  :name probe-definition)))))

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

(defmethod probe ((r resource) (property symbol))
  (let* ((os (host-os *host*))
	 (probe (or (find-probe r property os)
		    (error 'resource-probe-not-found
			   :resource r
			   :property property
			   :host *host*
			   :os os)))
	 (result (funcall (probe-generic-function probe) r os)))
    (when (eq +undefined+ (getf result property +undefined+))
      (error 'resource-probe-failed
	     :probe probe
	     :resource r
	     :property property
	     :host *host*
	     :os os))
    (setf (probed-properties r)
	  (append result (probed-properties r)))
    result))

(defmethod get-probed ((r resource) (property symbol))
  (let ((value (getf (probed-properties r) property +undefined+)))
    (when (eq +undefined+ value)
      (setq value (getf (probe r property) property +undefined+)))
    value))

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
