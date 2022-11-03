;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(defpackage :adams.system
  (:use :cl :asdf))

(in-package :adams.system)

(defsystem :adams
  :name "adams"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Remote system administration tools"
  :depends-on ("alexandria"
	       "chronicity"
	       "cl-base64"
	       "cl-debug"
	       "cl-ppcre"
	       "closer-mop"
	       "ironclad"
               "parse-number"
	       "re"
	       "str"
	       "trivial-utf-8")
  :components
  ((:file "package")
   (:module "shell" :depends-on ("package")
	    :components
	    ((:file "shell")
	     #+sbcl
	     (:file "sb-shell" :depends-on ("shell"))))
   (:module "core" :depends-on ("package" "shell")
            :components
            ((:file "defs")
             (:file "helpers")
             (:file "host"       :depends-on ("defs" "os" "resource-container"
                                                     "syntaxes"))
             (:file "include")
             (:file "operation"  :depends-on ("defs" "host" "properties"))
	     (:file "os")
	     (:file "probe"      :depends-on ("defs" "host" "properties"))
	     (:file "properties" :depends-on ("defs"))
	     (:file "resource"   :depends-on ("defs" "probe"))
	     (:file "resource-container" :depends-on ("defs"))
	     (:file "spec"       :depends-on ("defs" "resource"))
             (:file "syntaxes")))
   (:module "unix" :depends-on ("package" "shell" "core")
	    :components
	    ((:file "commands")
	     (:file "debian" :depends-on ("commands" "defs" "syntaxes"))
	     (:file "defs")
	     (:file "linux" :depends-on ("commands" "defs"))
	     (:file "openbsd" :depends-on ("commands" "defs"))
	     (:file "freebsd" :depends-on ("commands" "defs"))
	     (:file "darwin" :depends-on ("commands" "defs"))
	     (:file "operations" :depends-on ("commands" "defs"))
	     (:file "probes"  :depends-on ("commands" "defs"
                                           "stat" "syntaxes"))
             (:file "ssh" :depends-on ("defs"))
	     (:file "stat")
	     (:file "syntaxes")))))
