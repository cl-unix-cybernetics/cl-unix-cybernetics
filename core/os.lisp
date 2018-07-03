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

(defmethod match-specified-value ((host host) (property (eql :os)) specified probed)
  (re-match `(:sequence ,specified) probed))

;;  UNIX

(defclass os-unix (os) ())

;;  Linux

(defclass os-linux (os-unix)
  ((distrib :initarg :distrib
            :accessor os-distrib
            :type symbol)))

(defclass os-linux-debian (os-linux) ())

;;  BSD

(defclass os-bsd (os-unix) ())
(defclass os-freebsd (os-bsd) ())
(defclass os-openbsd (os-bsd) ())
(defclass os-darwin (os-bsd) ())

;;  Windows

(defclass os-windows (os) ())
