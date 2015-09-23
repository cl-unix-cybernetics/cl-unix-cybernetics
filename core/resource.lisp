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
	(compute-probes rc)
        (slot-value rc 'operations)
        (compute-operations rc)))

(defmethod slot-unbound (metaclass (rc resource-class) (slot-name (eql 'probes)))
  (closer-mop:finalize-inheritance rc)
  (slot-value rc 'probes))

;;  Resource

(defun resource-p (x)
  (typep x 'resource))

(defmethod print-object ((r resource) stream)
  (print-unreadable-object (r stream :type t :identity (not *print-pretty*))
    (format stream "~S ~D spec ~D probed" (resource-id r)
	    (/ (length (specified-properties r)) 2)
	    (/ (length (probed-properties r)) 2))))

(defun make-resource (type id &rest initargs &key &allow-other-keys)
  (apply #'make-instance type :id id initargs))

(defun resource-type (resource)
  (class-name (class-of resource)))

(defmethod resource-probes-properties ((res resource))
  (let ((properties))
    (dolist (probe (probes-of res))
      (dolist (property (probe-properties probe))
        (pushnew property properties)))
    (sort properties #'string<)))

#+nil
(resource-probes-properties (resource 'file "/"))

(defun probe-all-properties (res)
  (dolist (p (resource-probes-properties res))
    (get-probed res p))
  (probed-properties res))

#+nil
(probe-all-properties (resource 'file "/"))

(defmethod resource-operations-properties ((res resource))
  (let ((properties))
    (dolist (operation (operations-of res))
      (dolist (property (operation-properties operation))
        (pushnew property properties)))
    (sort properties #'string<)))

(defun pprint-plist (plist &optional (stream *standard-output*))
  (pprint-logical-block (stream plist)
    (iter (for* (k v) in plist)
          (for first-line-p initially t then nil)
          (unless first-line-p
            (pprint-newline :mandatory stream))
          (write k :stream stream)
          (write-char #\Space stream)
          (write v :stream stream))))

#+nil
(pprint-plist '(:a "aaa" :b "foo" :xyz "bar"))

(defmethod describe-probed-property-value ((resource t)
                                           (property t)
                                           value)
  value)

(defmethod describe-probed-property-value ((resource t)
                                           (property t)
                                           (value resource))
  (resource-id value))

(defmethod describe-probed-property-value ((resource t)
                                           (property t)
                                           (value list))
  (if (every #'resource-p value)
      (mapcar #'resource-id value)
      value))

(defmethod describe-probed% ((res resource) (out (eql :form)))
  (let* ((props (probe-all-properties res))
         (sorted-keys (sort (iter (for* (k v) in props)
                                  (collect k))
                            #'string<))
         (sorted-props (iter (for key in sorted-keys)
                             (for value = (describe-probed-property-value
                                           res key (get-property key props)))
                             (when value
                               (collect key)
                               (collect value)))))
    `(resource ',(class-name (class-of res))
               ,(resource-id res)
               ,@sorted-props)))

(defmethod describe-probed% ((res resource) (out null))
  (with-output-to-string (str)
    (describe-probed res str)))

(defmethod describe-probed% ((res resource) (out (eql t)))
  (describe-probed res *standard-output*))

(defmethod describe-probed% ((res resource) (out stream))
  (let ((form (describe-probed res :form))
        (*print-case* :downcase))
    (fresh-line out)
    (pprint-logical-block (out form :prefix "(" :suffix ")")
      (write (first form) :stream out)
      (write-char #\Space out)
      (write (second form) :stream out)
      (write-char #\Space out)
      (write (third form) :stream out)
      (pprint-indent :block 1 out)
      (pprint-newline :mandatory out)
      (pprint-plist (cdddr form) out))))

(defun describe-probed (resource &optional (output t))
  (describe-probed% resource output))

#+nil
(describe-probed (resource 'mount "/rd") t)


;;  Sync

(defmethod sync ((res resource))
  (when-let ((diff (resource-diff res)))
    (let* ((plist (resource-diff-to-plist diff))
           (host (current-host))
           (os (host-os host))
           (ops (list-operations res plist os))
           (sorted-ops (sort-operations ops)))
      (iter (for op in sorted-ops)
            (for op-keys = (operation-properties op))
            (for op-plist = (get-properties op-keys plist))
            (apply (operation-generic-function op)
                   res os op-plist)
            (clear-probed res op-keys)
            (for failed = (iter (for property in op-keys)
                                (for specified = (get-property property
                                                               op-plist))
                                (when (not (eq specified +undefined+))
                                  (for probed = (get-probed res property))
                                  (for desc = (describe-probed-property-value
                                               res property probed))
                                  (unless (match-specified-value
                                           res property specified desc)
                                    (collect property)
                                    (collect specified)
                                    (collect desc)))))
            (when failed
              (error 'resource-operation-failed
                     :diff failed
                     :operation op
                     :os os
                     :host host
                     :resource res))))))

(defmethod sync ((res resource-container))
  (call-next-method)
  (with-parent-resource res
    (iter (for child in (child-resources res))
          (sync child))))

(defmethod sync ((host host))
  (with-host host
    (call-next-method)))
