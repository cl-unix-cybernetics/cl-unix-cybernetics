;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2014 Thomas de Grivel <thomas@lowh.net>
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

;;  UNIX

(defclass os-unix (os)
  ())

;;  Linux

(defclass os-linux (os-unix)
  ())

;;  BSD

(defclass os-bsd (os-unix)
  ())

(defclass os-freebsd (os-bsd)
  ())

(defclass os-openbsd (os-bsd)
  ())

(defclass os-darwin (os-bsd)
  ())

;;  Windows

(defclass os-windows (os)
  ())

;;  OS detection

(defun uname ()
  (let ((uname-a (first (run "uname -a"))))
    (flet ((try-re (re)
	     (re-bind re (os-name node-name os-release os-version machine) uname-a)))
      (try-re #~"^(\S+) (\S+) (\S+) (.+) (\S+)$"))))

(defun make-os ()
  (multiple-value-bind (name hostname release version machine) (uname)
    (declare (ignore hostname))
    (let ((class (flet ((try (&rest parts)
			  (when-let ((s (find-symbol (string-upcase (str 'os- parts))
						     #.*package*)))
			    (ignore-errors (find-class s)))))
		   (or (try name '- release '- machine '- version)
		       (try name '- release '- machine)
		       (try name '- release '- version)
		       (try name '- release)
		       (try name '- machine '- version)
		       (try name '- machine)
		       (try name '- version)
		       (try name)
		       (error "Unknown OS : ~A" name)))))
      (make-instance class
		     :machine machine
		     :name name
		     :release release
		     :version version))))

(defun os ()
  (if (slot-boundp *host* 'os)
      #1=(slot-value *host* 'os)
      (setf #1# (make-os))))

(disable-re-syntax)
