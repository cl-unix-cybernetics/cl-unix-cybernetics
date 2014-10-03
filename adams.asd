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

(defpackage :adams.system
  (:use :cl :asdf))

(in-package :adams.system)

(defsystem :adams
  :name "adams"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.1"
  :description "Remote system administration tools"
  :depends-on ("alexandria"
	       "cl-base64"
	       "cl-debug"
	       "cl-ppcre"
	       "closer-mop"
	       "ironclad"
	       "iterate"
	       "trivial-utf-8")
  :components
  ((:file "package")
   (:module "shell" :depends-on ("package")
	    :components
	    ((:file "shell")
	     #+sbcl
	     (:file "sb-shell" :depends-on ("shell"))))
   (:file "resource" :depends-on ("shell"))
   (:file "manifest" :depends-on ("resource"))
   (:file "host" :depends-on ("manifest"))
   (:file "unix" :depends-on ("resource"))))