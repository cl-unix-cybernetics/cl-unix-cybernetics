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

;;  Operation methods

(declaim (ftype (function (operation) function) operation-generic-function))
(defgeneric operation-generic-function (operation))

(defmethod operation-generic-function ((op operation))
  (symbol-function (operation-name op)))

(defmethod print-object ((op operation) stream)
  (print-unreadable-object (op stream :type t :identity (not *print-pretty*))
    (format stream "~S (~{~A~^ ~})"
	    (operation-name op)
	    (operation-properties op))))

;;  Relate operations to properties in each resource class

(defmethod operation-class ((rc resource-class))
  'operation)

(defmethod compute-operations ((rc resource-class))
  (let ((class-precedence-list (closer-mop:class-precedence-list rc))
        (ops))
    (loop
       (when (endp class-precedence-list)
         (return))
       (let* ((class (pop class-precedence-list))
              (direct-ops (when (typep class 'resource-class)
                            (direct-operations class))))
         (dolist (op-definition direct-ops)
           (let ((op (apply #'make-instance (operation-class rc)
                            :name op-definition)))
             (push op ops)))))
    (nreverse ops)))

(defmethod operation-properties ((rc resource-class))
  (let ((properties nil))
    (dolist (op (operations-of rc))
      (dolist (property (operation-properties op))
	(pushnew property properties)))
    (sort properties #'string<)))

;;  Probing resources

(defmethod operation-properties ((r resource))
  (operation-properties (class-of r)))

(defmethod operations-of ((r resource))
  (operations-of (class-of r)))

(defmethod find-operation ((r resource)
                           (property symbol)
                           os)
  (some (lambda (op)
          (when (find property (operation-properties op) :test #'eq)
            (let ((f (operation-generic-function op)))
              (when (compute-applicable-methods f (list r os))
                op))))
        (operations-of r)))

(defmethod list-operations (res plist os)
  (let (operations)
    (loop
       (when (endp plist)
         (return))
       (let* ((property (pop plist))
              (value (pop plist))
              (op (find-operation res property os)))
         (declare (ignore value))
         (unless op
           (error 'resource-operation-not-found
                  :resource res
                  :property property
                  :host *host*
                  :os os))
         (pushnew (the operation op) operations)))
    (nreverse operations)))

(defgeneric resource-op-properties (resource))
(defgeneric op-property-before-p (resource p1 p2))
(defgeneric operation-before-p (resource op1 op2))
(defgeneric sort-operations (resource operations))

(defmethod resource-op-properties ((res resource))
  (op-properties (class-of res)))

(defmethod op-property-before-p ((res resource) (p1 symbol) (p2 symbol))
  (dolist (prop (resource-op-properties res))
    (cond ((endp prop) (return nil))
          ((eq p1 (first prop)) (return t))
          ((eq p2 (first prop)) (return nil)))
    (pop prop)))

(defmethod operation-before-p ((res resource) (op1 operation)
                               (op2 operation))
  (declare (type operation op1 op2))
  (let ((op1-properties (operation-properties op1)))
    (loop (when (endp op1-properties) (return))
       (let ((p1 (pop op1-properties))
             (op2-properties (operation-properties op2))
             (before-p t))
         (loop (when (endp op2-properties) (return))
            (let ((p2 (pop op2-properties)))
              (unless (op-property-before-p res p1 p2)
                (setf before-p nil)
                (return))))
         (when before-p
           (return-from operation-before-p t)))))
  (find op1 (the list (operations-before op2))))

(defmethod sort-operations ((res resource) (operations list))
  (sort operations (lambda (op1 op2)
                     (operation-before-p res op1 op2))))

(defmethod operate ((res resource) (plist list))
  (let* ((os (host-os (current-host)))
         (operations (list-operations res plist os))
         (sorted-ops (sort-operations res operations))
         (results))
    (loop
       (let* ((op (pop sorted-ops))
              (result (apply (operation-generic-function op)
                             res os plist)))
         (push result results)))
    (nreverse results)))

;;  Conditions

(defmethod print-object ((c resource-operation-not-found) stream)
  (if *print-escape*
      (call-next-method)
      (with-slots (resource property host os) c
	(format stream "Operation not found~%resource ~A~%property ~A~%host ~S~%~A"
		resource property (hostname host)
		(class-name (class-of os))))))

(defun print-list (x stream)
  (write-char #\( stream)
  (let ((first t))
    (dolist (item x)
      (if first
          (setq first nil)
          (write-char #\Space stream))
      (if (consp item)
          (print-list item stream)
          (prin1 item stream))))
  (write-char #\) stream))

(defun print-diff (stream diff)
  (dolist (item diff)
    (destructuring-bind (property expected probed) item
      (declare (type symbol property)
               (type list expected probed))
      (write-str stream property #\Newline
                 " expected ")
      (print-list expected stream)
      (write-str stream #\Newline
                 " probed   ")
      (print-list probed stream)
      (write-str stream #\Newline))))

(defmethod print-object ((c resource-operation-failed) stream)
  (if *print-escape*
      (call-next-method)
      (let ((*print-level*))
        (with-slots (operation resource diff host os) c
          (write-str stream (operation-name operation)
                     " failed for (resource '"
                     (string-downcase (class-name (class-of resource)))
                     " " (prin1-to-string (resource-id resource))
                     ") on (host " (prin1-to-string (hostname host))
                     ") " (class-name (class-of os))
                     "." #\Newline
                     "Conflicting values :" #\Newline)
          (print-diff stream diff)))))
