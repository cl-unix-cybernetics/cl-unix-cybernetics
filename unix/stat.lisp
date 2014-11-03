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

(defun mode-string-type (mode-string)
  (let ((c (char mode-string 0)))
    (or (car (find c +stat-mode-types+ :key #'second :test #'char=))
	(error "Unknown mode string type : ~S" c))))

(defun mode-type (mode)
  (let ((m (logand mode #o170000)))
    (or (car (find m +stat-mode-types+ :key #'third :test #'=))
	(error "Unknown mode type : #o~O." m))))

(defun type-mode (type)
  (or (third (find type +stat-mode-types+ :key #'car :test #'eq))
      (error "Unknown type ~S." type)))

(defun type-mode-char (type)
  (or (second (find type +stat-mode-types+ :key #'car :test #'eq))
      (error "Unknown type ~S." type)))

(defun mode-string (mode)
  (str (type-mode-char (mode-type mode))
       (if (logtest     #o0400 mode) #\r #\-)
       (if (logtest     #o0200 mode) #\w #\-)
       (if (logtest     #o0100 mode)
	   (if (logtest #o4000 mode) #\s #\x)
	   (if (logtest #o4000 mode) #\S #\-))
       (if (logtest     #o0040 mode) #\r #\-)
       (if (logtest     #o0020 mode) #\w #\-)
       (if (logtest     #o0010 mode)
	   (if (logtest #o2000 mode) #\s #\x)
	   (if (logtest #o2000 mode) #\S #\-))
       (if (logtest     #o0004 mode) #\r #\-)
       (if (logtest     #o0002 mode) #\w #\-)
       (if (logtest     #o0001 mode)
	   (if (logtest #o1000 mode) #\s #\x)
	   (if (logtest #o1000 mode) #\S #\-))))

(defun parse-mode-string (s)
  (declare (type (string 10) s))
  (logior
   (ecase (char s 0) (#\- 0) (#\r #o0400))
   (ecase (char s 1) (#\- 0) (#\w #o0200))
   (ecase (char s 2) (#\- 0) (#\x #o0100) (#\S #o4000) (#\s #o4100))
   (ecase (char s 3) (#\- 0) (#\r #o0040))
   (ecase (char s 4) (#\- 0) (#\w #o0020))
   (ecase (char s 5) (#\- 0) (#\x #o0010) (#\S #o2000) (#\s #o2010))
   (ecase (char s 6) (#\- 0) (#\r #o0004))
   (ecase (char s 7) (#\- 0) (#\w #o0002))
   (ecase (char s 8) (#\- 0) (#\x #o0001) (#\S #o1000) (#\s #o1001))))

(defun parse-unix-timestamp (x)
  (let ((n (typecase x
	     (string (parse-integer x))
	     (integer x))))
    (local-time:unix-to-timestamp n)))
