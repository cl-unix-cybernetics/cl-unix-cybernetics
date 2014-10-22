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

(enable-re-syntax)

;;  Simple regexp-based parser generator with ITERATE support

(defmacro define-syntax (name vars re &body body)
  (let ((parse-name (sym 'parse- name))
	(doc (when (stringp (first body)) (pop body)))
	(values (or (first (last body))
		    `(values ,@(iter (for spec in vars)
				     (if (consp spec)
					 (dolist (var (cdr spec))
					   (collect var))
					 (collect spec)))))))
    `(progn
       (defun ,parse-name (line)
	 ,@(when doc (list doc))
	 (re-bind ,re ,vars line
	   ,@(or body `(,values))))
       (iterate:defmacro-clause (,name iter-vars in lines)
	 ,@(when doc (list doc))
	 (let ((line (gensym ,(format nil "~A-LINE-" (symbol-name name)))))
	   `(progn (for ,line in ,lines)
		   (for (values ,@iter-vars) = (,',parse-name ,line))))))))

;;  Standard commands

(defun grep (pattern &rest files)
  (run "grep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun egrep (pattern &rest files)
  (run "egrep ~A~{ ~A~}" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun stat (options &rest files)
  (run "stat ~A~{ ~A~}" options (mapcar #'sh-quote files)))

(defun ls (options &rest files)
  (run "ls ~A~{ ~A~}" options (mapcar #'sh-quote files)))

;;  Group

(define-resource-class group () ()
  ((probe-group-in-/etc/group :properties (name passwd gid members))))

(define-syntax group<5> (name passwd
			 (#'parse-integer gid)
			 ((lambda (m) (cl-ppcre:split "," m)) members))
  #~|^([^:]*):([^:]*):([^:]*):([^:]*)$|
  "Syntax of the group permissions file /etc/group. See group(5).")

(defmethod probe-group-in-/etc/group ((group group) (os os-unix))
  (let ((id (resource-id group)))
    (iter (group<5> (name passwd gid members) in (grep (str id) "/etc/group"))
	  (when (etypecase id
		  (integer (= id gid))
		  (string (string= id name)))
	    (return (list 'name name
			  'passwd passwd
			  'gid gid
			  'members members))))))

;;  User

(define-resource-class user ()
  ()
  ((probe-user-in-/etc/passwd :properties (login uid gid realname home shell))
   (probe-user-groups-in-/etc/group :properties (groups))))

(define-syntax passwd<5> (name pass
			  (#'parse-integer uid gid)
			  realname home shell)
  #~|^([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*)$|
  "Syntax for the password file /etc/passwd. See passwd(5).")

(defmethod probe-user-in-/etc/passwd ((user user) (os os-unix))
  (let ((id (resource-id user)))
    (iter (passwd<5> (login pass uid gid realname home shell)
		     in (etypecase id
			  (integer (grep (str #\: id #\:) "/etc/passwd"))
			  (string (egrep (str #\^ id #\:) "/etc/passwd"))))
	  (when (etypecase id
		  (string (string= id login))
		  (integer (= id uid)))
	    (return (list 'login login 'uid uid 'gid gid
			  'realname realname 'home home 'shell shell))))))

(defmethod probe-user-groups-in-/etc/group ((user user) (os os-unix))
  (let* ((id (resource-id user))
	 (user-login (if (stringp id)
			 id
			 (get-probed user 'login)))
	 (user-gid (get-probed user 'gid)))
    (iter (group<5> (name passwd gid members) in (grep user-login
						       "/etc/group"))
	  (with user-group = nil)
	  (cond ((= user-gid gid) (setq user-group name))
		((find user-login members :test #'string=) (collect name into groups)))
	  (finally (let ((groups (sort groups #'string<)))
		     (return (list 'groups (if user-group
					       (cons user-group groups)
					       groups))))))))

;;  st_mode, see stat(2)

(define-constant +stat-mode-types+
    '((fifo              #\p #o010000)
      (character-special #\c #o020000)
      (directory         #\d #o040000)
      (block-special     #\b #o060000)
      (file              #\- #o100000)
      (symbolic-link     #\l #o120000)
      (socket            #\s #o140000))
  :test #'equal)

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

;;  Filesystem nodes

(define-resource-class vnode ()
  ()
  ((probe-vnode-using-ls :properties (mode links owner group size mtime))
   (probe-vnode-using-stat :properties (type permissions owner group size
					     atime mtime ctime blocks))))

(defun make-one-second-span (time)
  (let ((y   (chronicity:year-of   time))
	(m   (chronicity:month-of  time))
	(d   (chronicity:day-of    time))
	(h   (chronicity:hour-of   time))
	(min (chronicity:minute-of time))
	(sec (chronicity:sec-of    time)))
    (make-instance 'chronicity:span
		   :start (chronicity:make-datetime y m d h min sec)
		   :end   (chronicity:make-datetime y m d h min (1+ sec)))))

(define-syntax ls<1>-lT (mode
			 (#'sh-parse-integer links)
			 owner
			 group
			 (#'sh-parse-integer size)
			 (#'chronicity:parse time)
			 name)
  #~|^([-a-zA-Z]{10})\s+([0-9]+)\s+(\S+)\s+(\S+)\s+([0-9]+)\s+(\S+ \S+ \S+ \S+)\s+(.+)$|
  "Syntax for `ls -lT` output. See ls(1)."
  (values name mode links owner group size time))

(defmethod probe-vnode-using-ls ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (iter (ls<1>-lT (name mode links owner group size mtime)
		    in (ls "-ldT" id))
	  (when (string= id name)
	    (return (list 'mode mode
			  'links links
			  'owner owner
			  'group group
			  'size size
			  'mtime (make-one-second-span mtime)))))))

(defun parse-unix-timestamp (x)
  (let ((n (typecase x
	     (string (parse-integer x))
	     (integer x))))
    (local-time:unix-to-timestamp n)))

(define-syntax stat<1>-r ((#'sh-parse-integer
			   dev ino mode links uid gid rdev size)
			  (#'parse-unix-timestamp atime mtime ctime)
			  (#'sh-parse-integer blksize blocks flags)
			  file)
    #~|^([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) (.+)$|
  "Syntax for raw stat(1) output."
  (values file dev ino mode links uid gid rdev size
	  atime mtime ctime blksize blocks flags))

(defmethod probe-vnode-using-stat ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (iter (stat<1>-r (name dev ino mode links uid gid rdev size
			   atime mtime ctime blksize blocks flags)
		     in (stat "-r" id))
	  (when (string= id name)
	    (return (list 'dev dev
			  'ino ino
			  'mode (mode-string mode)
			  'links links
			  'uid uid
			  'gid gid
			  'rdev rdev
			  'size size
			  'atime atime
			  'mtime mtime
			  'ctime ctime
			  'blksize blksize
			  'blocks blocks
			  'flags flags))))))

;;  Regular file

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *cksum-algorithms*
    '(cksum md4 md5 rmd160 sha1 sha224 sha256 sha384 sha512 sum sysvsum)))

(define-resource-class file (vnode) ()
  #.(iter (for algorithm in *cksum-algorithms*)
	  (collect `(,(sym 'probe-file-cksum- algorithm)
		      :properties (,algorithm)))))

(define-syntax cksum<1> (algo sum file)
    #~|(\S+) \((.*)\) = (\S+)|
  "Syntax for cksum(1) output.")

#.(cons 'progn
	(iter (for algorithm in *cksum-algorithms*)
	      (for name = (sym 'probe-file-cksum- algorithm))
	      (collect `(defgeneric ,name (file)))
	      (collect `(defmethod ,name ((file file))
			  (let ((id (resource-id file)))
			    (iter (cksum<1> (algo name sum)
					    in (run "cksum -a ~A ~A"
						    ',algorithm
						    (sh-quote id)))
				  (when (and (string= ',algorithm algo)
					     (string= id name))
				    (return (list ',algorithm sum)))))))))

(disable-re-syntax)
