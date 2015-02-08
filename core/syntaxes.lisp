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

;;  Host

(defun parse-uptime (string)
  (or (re-bind #~|^\s*([0-9]+ days,\s*)?([0-9]+):([0-9]+)\s*$| (d h m) string
        (* 60 (+ (parse-integer m)
                 (* 60
                    (+ (parse-integer h)
                       (if d
                           (* 24 (parse-integer d :junk-allowed t))
                           0))))))
      (error "Invalid uptime ?")))

(define-syntax uptime<1> ((#'chronicity:parse time)
                          (#'parse-uptime uptime)
                          (#'parse-integer users)
                          (#'parse-number load1 load5 load15))
  #~|^\s*(\S+)\s+up\s+(.+), ([0-9]+) users?, load averages: ([0-9.]+), ([0-9.]+), ([0-9.]+)$|
  "Syntax of the group permissions file /etc/group. See group(5).")
