
(in-package :cl-user)

(require :adams)

(in-package :adams-user)

;; TEST

#+nil
(untrace shell-status)

(setf (debug-p :shell) t)

(with-host "h"
  (run "hostname && false")
  (run "pwd")
  (run "ls")
  (run "exit"))

(with-manifest "h"
  (make-instance 'user :name "vmail"
		 :shell "/bin/ksh"
		 :home "/var/qmail/domains"
		 :gid 13000
		 :uid 13000))

(adams::apply-manifest "h")

(manifest-resources (manifest "h"))

(apply-manifest "h")

(remove-manifest "h")
