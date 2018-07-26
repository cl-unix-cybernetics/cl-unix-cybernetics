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

;;  st_mode, see stat(2)

(define-constant +stat-mode-types+
    '((fifo              #\p #o010000)
      (character-special #\c #o020000)
      (directory         #\d #o040000)
      (block-special     #\b #o060000)
      (file              #\- #o100000)
      (symbolic-link     #\l #o120000)
      (socket            #\s #o140000))
  :test #'equalp)

(defun octal (number)
  (format nil "0~O" number))

(defclass mode ()
  ((fixnum :initarg :fixnum
           :initform 0
           :type fixnum
           :reader mode-fixnum)))

(defgeneric mode-type (mode))
(defgeneric mode-string (mode))
(defgeneric mode-fixnum (mode))
(defgeneric mode-octal (mode))
(defgeneric mode (value))

(defmethod mode-type ((mode fixnum))
  (let ((type (logand #o170000 mode)))
    (unless (zerop type)
      type)))

(defmethod mode-type ((mode mode))
  (mode-type (mode-fixnum mode)))

(defmethod mode-permissions ((mode mode))
  (logand #o007777 (mode-fixnum mode)))

(defmethod mode-permissions (mode)
  (mode-permissions (mode mode)))

(defmethod mode-string ((mode mode))
  (let* ((num (mode-fixnum mode))
         (type (mode-type num)))
    (str (when type
           (second (find type +stat-mode-types+ :key #'third)))
         (if (logtest     #o0400 num) #\r #\-)
         (if (logtest     #o0200 num) #\w #\-)
         (if (logtest     #o0100 num)
             (if (logtest #o4000 num) #\s #\x)
             (if (logtest #o4000 num) #\S #\-))
         (if (logtest     #o0040 num) #\r #\-)
         (if (logtest     #o0020 num) #\w #\-)
         (if (logtest     #o0010 num)
             (if (logtest #o2000 num) #\s #\x)
             (if (logtest #o2000 num) #\S #\-))
         (if (logtest     #o0004 num) #\r #\-)
         (if (logtest     #o0002 num) #\w #\-)
         (if (logtest     #o0001 num)
             (if (logtest #o1000 num) #\s #\x)
             (if (logtest #o1000 num) #\S #\-)))))

(defmethod mode-string (mode)
  (mode-string (mode mode)))

(defmethod mode-fixnum (mode)
  (mode-fixnum (mode mode)))

(defmethod mode-octal (mode)
  (octal (mode-fixnum mode)))

(defun parse-mode-string (s)
  (let ((type (when (= 10 (length s))
                (let ((c (char s 0)))
                  (setq s (subseq s 1))
                  (or (find c +stat-mode-types+ :key #'cadr :test #'char=)
                      (error "Unknown mode type : ~C" c))))))
    (make-instance
     'mode :fixnum
     (logior
      (if type (third type) 0)
      (ecase (char s 0) (#\- 0) (#\r #o0400))
      (ecase (char s 1) (#\- 0) (#\w #o0200))
      (ecase (char s 2) (#\- 0) (#\x #o0100) (#\S #o4000) (#\s #o4100))
      (ecase (char s 3) (#\- 0) (#\r #o0040))
      (ecase (char s 4) (#\- 0) (#\w #o0020))
      (ecase (char s 5) (#\- 0) (#\x #o0010) (#\S #o2000) (#\s #o2010))
      (ecase (char s 6) (#\- 0) (#\r #o0004))
      (ecase (char s 7) (#\- 0) (#\w #o0002))
      (ecase (char s 8) (#\- 0) (#\x #o0001) (#\S #o1000) (#\s #o1001))))))

(defmethod mode ((mode mode))
  mode)

(defmethod mode ((n fixnum))
  (make-instance 'mode :fixnum n))

(defmethod mode ((s string))
  (if (char<= #\0 (char s 0) #\9)
      (make-instance 'mode :fixnum (parse-integer s :radix 8))
      (parse-mode-string s)))

(defmethod mode ((x null))
  (make-instance 'mode :fixnum 0))

(defmethod print-object ((mode mode) stream)
  (prin1 `(mode ,(if (mode-type mode)
                     (mode-string mode)
                     (mode-octal mode)))
         stream))

(defun parse-unix-timestamp (x)
  (let ((n (typecase x
	     (string (parse-integer x))
	     (integer x))))
    (local-time:unix-to-timestamp n)))

(defmethod describe-probed-property-value (resource property (mode mode))
  (mode-octal mode))

(defmethod compare-property-values ((resource vnode) (property (eql :mode))
                                    value1 value2)
  (= (mode-fixnum value1) (mode-fixnum value2)))

(defmethod match-specified-value ((resource vnode) (property (eql :mode))
                                  specified probed)
  (= (mode-fixnum specified) (mode-fixnum probed)))
