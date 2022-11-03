;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(defmethod print-object ((os os) stream)
  (print-unreadable-object (os stream :type t :identity (not *print-pretty*))
    (with-slots (machine name release version) os
    (format stream "~A ~A ~A ~A"
	    machine name release version))))

(defmethod describe-probed-property-value ((resource host)
                                           (property (eql :os))
                                           (os os))
  (with-slots (machine name release version) os
    (format nil "~A ~A ~A ~A"
	    machine name release version)))

(defmethod match-specified-value ((host host) (property (eql :os)) specified probed os)
  (re-match `(:sequence ,specified) probed))

;;  UNIX

(defclass os-unix (os) ())

;;  Linux

(defclass os-linux (os-unix)
  ((distrib :initarg :distrib
            :accessor os-distrib
            :type symbol)))
(defclass os-linux-debian (os-linux) ())
(defclass os-linux-gentoo (os-linux) ())

;;  BSD

(defclass os-bsd (os-unix) ())
(defclass os-freebsd (os-bsd) ())
(defclass os-openbsd (os-bsd) ())
(defclass os-darwin (os-bsd) ())

;;  Windows

(defclass os-windows (os) ())
