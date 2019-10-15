;;
;;  adams - system administrator written in Common Lisp
;;
;;  Copyright 2013,2014,2018 Thomas de Grivel <thoxdg@gmail.com>
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
  (declare (type resource resource))
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
  (declare (type resource res))
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
    (let ((first-line-p t))
      (loop
         (when (endp plist)
           (return))
         (let ((k (pop plist))
               (v (pop plist)))
           (unless first-line-p
             (pprint-newline :mandatory stream))
           (setq first-line-p nil)
           (write k :stream stream)
           (write-char #\Space stream)
           (write v :stream stream))))))

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

(defun plist-keys (plist)
  (declare (type list plist))
  (let ((keys))
    (loop
       (when (endp plist)
         (return))
       (let ((key (pop plist))
             (value (pop plist)))
         (declare (ignore value))
         (push key keys)))
    keys))

(declaim (ftype (function (list) list) plist-keys))

(defmethod describe-probed% ((res resource) (out (eql :form)))
  (let* ((props (probe-all-properties res))
         (sorted-keys (sort (plist-keys props) #'string<))
         (sorted-props (mapcan (lambda (key)
                                 (let ((value (describe-probed-property-value
                                               res key (get-property key props))))
                                   (when value
                                     (list key value))))
                                sorted-keys)))
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
  (declare (type resource resource))
  (describe-probed% resource output))

#+nil
(describe-probed (resource 'mount "/rd") t)

(defmethod resource-additional-specs ((res resource) (os t))
  )

;;  Sync

(defun sync-check (host res op op-keys op-plist os)
  (let ((failed))
    (dolist (property op-keys)
      (let ((specified (get-property property op-plist)))
        (when (not (eq specified +undefined+))
          (let* ((probed (get-probed res property))
                 (desc (describe-probed-property-value res property
                                                       probed)))
            (unless (match-specified-value res property specified desc)
              (push (list property specified desc) failed))))))
    (setq failed (nreverse failed))
    (when failed
      (error 'resource-operation-failed
             :diff failed
             :operation op
             :os os
             :host host
             :resource res))))

(defmethod sync ((res resource))
  (when-let ((diff (resource-diff res)))
    (let* ((plist (resource-diff-to-plist diff))
           (host (current-host))
           (os (host-os host))
           (ops (list-operations res plist os))
           (sorted-ops (sort-operations res ops)))
      (loop
         (when (endp sorted-ops)
           (return))
         (let* ((op (pop sorted-ops))
                (op-keys (operation-properties op))
                (op-plist (get-properties op-keys plist))
                (op-fun (operation-generic-function op)))
           (apply (the function op-fun) res os op-plist)
           (clear-probed res op-keys)
           (sync-check host res op op-keys op-plist os))))))

(defmethod sync ((host host))
  (with-host host
    (resource-additional-specs host (host-os host))
    (call-next-method)))
