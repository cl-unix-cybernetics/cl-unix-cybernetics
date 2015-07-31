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

;;  Properties implemented as property lists

(defun properties (&rest keys-and-values)
  (apply #'list keys-and-values))

(defmacro properties* (&rest vars)
  (when (and (consp (first vars))
             (endp (rest vars)))
    (setq vars (first vars)))
  `(list ,@(iter (for v in vars)
                 (collect (make-keyword v))
                 (collect v))))

(defmacro values* (&rest vars)
  (when (and (consp (first vars))
             (endp (rest vars)))
    (setq vars (first vars)))
  `(values ,@vars))

(defmacro get-property (property properties)
  `(getf ,properties ,property +undefined+))

(defun get-properties (keys properties)
  (iter (for k in keys)
        (for v = (get-property k properties))
        (unless (eq v +undefined+)
          (collect k)
          (collect v))))

(defmethod compare-property-values (resource property value1 value2)
  (equalp value1 value2))

(defmethod merge-property-values (resource property old new)
  (unless (compare-property-values resource property old new)
    (warn "Conflicting values for property ~A in
~A
 old: ~S
 new: ~S
Keeping old value by default."
          property resource old new)
    old))

#+nil ;; Is this really the right thing ?
(defmethod merge-property-values ((resource t)
                                  (property t)
                                  (old list)
                                  (new list))
  (append old new))

(defun merge-properties (resource &rest properties)
  (let* ((result (cons nil nil))
         (tail result))
    (dolist (p properties)
      (iter
        (for* (key val) in p)
        (let ((result-val (getf (cdr result) key +undefined+)))
          (cond ((eq result-val +undefined+)
                 (setf (cdr tail) (list key val)
                       tail (cddr tail)))
                ((not (equalp result-val val))
                 (setf (getf (cdr result) key)
                       (merge-property-values resource key result-val val)))))))
    (cdr result)))

(let ((p ()))
  (setf (get-property :os p) "OpenBSD")
  p)
