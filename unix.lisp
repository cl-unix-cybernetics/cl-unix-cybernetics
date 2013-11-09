;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2013 Thomas de Grivel <billitch@gmail.com>
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

;;  Timestamp

(defconstant +timestamp-offset+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun timestamp-to-universal-time (timestamp)
  (+ timestamp +timestamp-offset+))

(defun universal-time-to-timestamp (ut)
  (- ut +timestamp-offset+))

;;  Standard commands

(defun grep (pattern &rest files)
  (run "grep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun egrep (pattern &rest files)
  (run "egrep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun stat (&rest files)
  (run "stat -r~{ ~A~}" (mapcar #'sh-quote files)))

;;  Group

(define-resource-class group ()
  ((passwd :type string)
   (gid :type fixnum)
   (members :type list)))

(defmacro define-syntax (name vars regex &body body)
  (let ((parse-name (intern (format nil "PARSE-~A" (symbol-name name))))
	(doc (when (stringp (car body)) (pop body)))
	(values (or (car (last body))
		    `(values ,@(mapcan (lambda (v) (if (consp v)
						       (copy-list (cdr v))
						       (cons v nil)))
				       vars)))))
    `(progn
       (defun ,parse-name (line)
	 ,@(when doc (list doc))
	 (cl-ppcre:register-groups-bind ,vars (,regex line)
	   ,@(or body `(,values))))
       (defmacro-clause (,name vars IN lines)
	 ,@(when doc (list doc))
	 (let ((line (gensym (format nil "~A-LINE-" ,(symbol-name name)))))
	   `(progn (for ,line in ,lines)
		   (for (values ,@vars) = (,',parse-name ,line))))))))

(define-syntax group<5> (name passwd
			 (#'parse-integer gid)
			 ((lambda (m) (cl-ppcre:split "," m)) members))
  "^([^:]*):([^:]*):([^:]*):([^:]*)$"
  "Syntax of the group permissions file /etc/group. See group(5).")

(defmethod gather-resource ((resource group) name)
  (iter (group<5> (name* passwd* gid* members*) in (grep name "/etc/group"))
    (when (string= name name*)
      (with-slots (passwd gid members) resource
	(setf passwd passwd* gid gid* members members*))
      (return resource))))

(defun gather-gid-group-name (gid)
  (iterate (group<5> (name* passwd* gid* members*)
		      in (grep (format nil ":~A:" gid) "/etc/group"))
	   (when (= gid gid*)
	     (return name*))))

;;  User

(define-resource-class user ()
  ((uid :type fixnum)
   (gid :type fixnum)
   (groups :type list)
   (realname :type string)
   (home :type string)
   (shell :type string)))

(define-syntax passwd<5> (name pass
			  (#'parse-integer uid gid)
			  realname home shell)
  "^([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*)$"
  "Syntax for the password file /etc/passwd. See passwd(5).")

(defun gather-user-groups (user user-gid)
  (iter (group<5> (name* passwd* gid* members*)
		  in (grep user "/etc/group"))
	(with user-group = nil)
	(cond ((= user-gid gid*) (setq user-group name*))
	      ((find user members* :test #'string=) (collect name* into groups)))
	(finally (return (if user-group
			     (cons user-group groups)
			     groups)))))

(defmethod gather-resource ((resource user) name)
  (iter (passwd<5> (name* pass* uid* gid* realname* home* shell*)
		   in (grep name "/etc/passwd"))
	(when (string= name name*)
	  (with-slots (uid gid groups realname home shell) resource
	    (setf uid uid* gid gid* realname realname* home home* shell shell*
		  groups (gather-user-groups name gid*)))
	  (return resource))))

(defun gather-uid-user-name (uid)
  (iterate (passwd<5> (name* pass* uid* gid* realname* home* shell*)
		      in (grep (format nil ":~D:" uid) "/etc/passwd"))
	   (when (= uid uid*)
	     (return name*))))

;;  File

(define-constant +file-type-mode-bits+
    '((:fifo              . #o010000)
      (:character-special . #o020000)
      (:directory         . #o040000)
      (:block-special     . #o060000)
      (:file              . #o100000)
      (:link              . #o120000)
      (:socket            . #o140000))
  :test 'equalp)

(defun mode-file-type (mode)
  (car (rassoc (logand mode #o170000) +file-type-mode-bits+)))

(defun mode-permissions (mode)
  (let ((s (make-string 9)))
    (setf (char s 0) (if (logtest     #o0400 mode) #\r #\-))
    (setf (char s 1) (if (logtest     #o0200 mode) #\w #\-))
    (setf (char s 2) (if (logtest     #o0100 mode)
			 (if (logtest #o4000 mode) #\s #\x)
			 (if (logtest #o4000 mode) #\S #\-)))
    (setf (char s 3) (if (logtest     #o0040 mode) #\r #\-))
    (setf (char s 4) (if (logtest     #o0020 mode) #\w #\-))
    (setf (char s 5) (if (logtest     #o0010 mode)
			 (if (logtest #o2000 mode) #\s #\x)
			 (if (logtest #o2000 mode) #\S #\-)))
    (setf (char s 6) (if (logtest     #o0004 mode) #\r #\-))
    (setf (char s 7) (if (logtest     #o0002 mode) #\w #\-))
    (setf (char s 8) (if (logtest     #o0001 mode)
			 (if (logtest #o1000 mode) #\s #\x)
			 (if (logtest #o1000 mode) #\S #\-)))
    s))

(define-syntax stat<1> ((#'sh-parse-integer dev ino mode nlink uid gid
					    rdev size atime mtime ctime
					    blksize blocks flags)
			file)
  "^([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) (.+)$"
  "Syntax for raw stat(1) output."
  (values file dev ino mode nlink uid gid rdev size
	  atime mtime ctime blksize blocks flags))

(define-resource-class file ()
  ((type :type symbol)
   (permissions :type string)
   (owner :type (or string fixnum))
   (group :type (or string fixnum))
   (size :type integer)
   (atime :type integer)
   (mtime :type integer)
   (ctime :type integer)
   (blocks :type integer)))

(defmethod gather-resource ((resource file) name)
  (iterate (stat<1> (name* dev* ino* mode* nlink* uid* gid* rdev* size*
	             atime* mtime* ctime* blksize* blocks* flags*)
		    in (stat name))
	   (when (string= name name*)
	     (with-slots (type permissions owner group
			  size atime mtime ctime blocks) resource
	       (setf type (mode-file-type mode*)
		     permissions (mode-permissions mode*)
		     owner (gather-uid-user-name uid*)
		     group (gather-gid-group-name gid*)
		     size size* atime atime* mtime mtime* ctime ctime*
		     blocks blocks*))
	    (return resource))))

(defun permissions-mode-bits (s)
  (declare (type (string 9) s))
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
