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

(in-re-readtable)

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

;;  Syntaxes

(define-syntax group<5> (name passwd
			 (#'parse-integer gid)
			 ((lambda (m) (cl-ppcre:split "," m)) members))
  #~|^([^:]*):([^:]*):([^:]*):([^:]*)$|
  "Syntax of the group permissions file /etc/group. See group(5).")

(define-syntax passwd<5> (name pass
			  (#'parse-integer uid gid)
			  realname home shell)
  #~|^([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*):([^:]*)$|
  "Syntax for the password file /etc/passwd. See passwd(5).")

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

(define-syntax stat<1>-r ((#'sh-parse-integer
			   dev ino mode links uid gid rdev size)
			  (#'parse-unix-timestamp atime mtime ctime)
			  (#'sh-parse-integer blksize blocks flags)
			  file)
    #~|^([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+) (.+)$|
  "Syntax for raw stat(1) output."
  (values file dev ino mode links uid gid rdev size
	  atime mtime ctime blksize blocks flags))

(define-syntax cksum<1> (algo sum file)
    #~|(\S+) \((.*)\) = (\S+)|
  "Syntax for cksum(1) output.")
