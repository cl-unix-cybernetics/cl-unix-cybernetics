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

;;  Specified properties, what all specifications amount to.

(defmethod get-specified ((res resource)
                          (property symbol))
  (let ((value (get-property property (specified-properties res))))
    (if (eq +undefined+ value)
        (values nil +undefined+)
        value)))

(defmethod (setf get-specified) (value
                                 (res resource)
                                 (property symbol))
  (let ((p (specified-properties res)))
    (setf (get-property property p) value)
    (setf (specified-properties res) p)))

;;  Parse specifications

(defmethod parse-next-specification ((res resource) spec)
  (when (and (symbolp (first spec))
             (consp (rest spec)))
    (let ((property (pop spec))
          (value (pop spec)))
      (setf (get-specified res property) value)
      spec)))

(defmethod parse-next-specification ((res resource-container) spec)
  (cond ((typep (first spec) 'resource)
         (let ((child (pop spec)))
           (add-resource res child))
	 spec)
	(:otherwise (call-next-method))))

(defmethod parse-specification ((res resource) (spec null))
  res)

(defmethod parse-specification ((res resource) (spec cons))
  (loop
     (when (endp spec)
       (return))
     (typecase (first spec)
       (cons (parse-specification res (first spec))
             (setq spec (rest spec)))
       (t (let ((next-spec (parse-next-specification res spec)))
            (when (eq spec next-spec)
              (error "Invalid specification : ~S" spec))
            (setq spec next-spec)))))
  res)

#+nil
(parse-specification *localhost*
                     '(:hostname "arrakis.lowh.net"))

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
  (let ((res (or #1=(get-resource type id)
                 (setf #1# (make-resource type id)))))
    (when spec
      (parse-specification res spec))
    res))

(defmacro specify (&body specification)
  `(macrolet ,(mapcar (lambda (c)
			`(,(class-name c) (id &body s)
			   `(resource ',',(class-name c) ,id ,@s)))
		      (subclasses (find-class 'resource)))
     (parse-specification *localhost*
			  (list ,@specification))))

#+nil
(specify (user "billitch" :uid 19256 :group (group "billitch")))

;;  Methods for matching specified and probed values

(defgeneric match-specified-value (resource property specified probed os))

(defmethod match-specified-value (resource property specified probed os)
  (equalp specified (describe-probed-property-value resource property probed)))

(defmethod match-specified-value (resource (property (eql :ensure))
                                  (specified (eql :present))
                                  (probed null)
                                  os)
  t)

;;  Methods to get current status of resource

(defgeneric resource-diff (resource)
  (:documentation "Two values are returned :
First value lists properties out of specification in the following format :
  (PROPERTY-NAME SPECIFIED-VALUE PROBED-VALUE).
Second value lists properties in line with spec. Format is
  (PROPERTY-NAME VALUE)"))

(defmethod resource-diff ((res resource))
  (let ((specified-properties (specified-properties res))
        diff)
    (loop
       (when (endp specified-properties)
         (return))
       (let* ((property (pop specified-properties))
              (specified (pop specified-properties))
              (probed (get-probed res property))
              (desc (describe-probed-property-value res property probed)))
         (unless (match-specified-value res property specified desc (host-os *host*))
           (push `(,property ,specified ,desc) diff))))
    (nreverse diff)))

#+nil
(resource-diff (resource 'directory "/" :owner "root" :uid 0))

(defmethod resource-diff ((res resource-container))
  (with-parent-resource res
    (let ((diffs))
      (do-resources (r) res
        (let ((d (resource-diff r)))
          (when d
            (push (cons r d) diffs))))
      (append (call-next-method res)
              (sort diffs #'resource-before-p :key #'first)))))

(defmethod resource-diff ((host host))
  (with-host host
    (call-next-method)))

(defun resource-diff-to-plist (diff)
  (let ((plist))
    (loop
       (when (endp diff)
         (return))
       (let* ((item (pop diff))
              (key (first item)))
         (when (keywordp key)
           (push key plist)
           (push (second item) plist))))
    (nreverse plist)))
