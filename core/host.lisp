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

;;  Host

(defun hostname (host)
  (resource-id (the host host)))

#+nil
(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity t)
    (write-string (hostname host) stream)))

;;  Host shell

(defmethod host-shell ((host host))
  (if (slot-boundp host 'shell)
      (slot-value host 'shell)
      (setf (slot-value host 'shell) (host-connect host))))

(defmethod (setf host-shell) ((shell shell) (host host))
  (setf (slot-value host 'shell) shell))

(defmethod host-disconnect ((host host))
  (when (slot-boundp host 'shell)
    (shell-close (host-shell host))
    (slot-makunbound host 'shell)))

(defmacro with-connected-host ((var hostname) &body body)
  (let ((g!host (gensym "HOST-")))
    `(let ((,g!host (make-instance 'ssh-host :id ,hostname)))
       (unwind-protect (let ((,var ,g!host)) ,@body)
	 (host-disconnect ,g!host)))))

(defmethod host-run ((host host) (command string) &rest format-args)
  (let ((shell (host-shell host)))
    (when (shell-closed-p shell)
      (setq shell (host-connect host)))
    (apply #'shell-run shell command format-args)))

(defmethod host-run ((hostname string) command &rest format-args)
  (with-connected-host (host hostname)
    (apply #'host-run host command format-args)))

;;  localhost

(defvar *localhost* (load-time-value
		     (make-instance 'host
				    :id "localhost")))

(defmethod host-connect ((host (eql *localhost*)))
  (setf (host-shell host) (make-shell)))

;;  SSH host

(defmethod host-connect ((host ssh-host))
  (setf (host-shell host) (make-shell "/usr/bin/ssh" (hostname host))))

;;  High level API

(defvar *host* *localhost*)

(defmacro with-host (hostname &body body)
  `(with-connected-host (*host* ,hostname)
     (let ((*manifest* (host-manifest *host*)))
       ,@body)))

(defun run (command &rest format-args)
  (apply #'host-run *host* command format-args))

;;  OS

(defmethod print-object ((os os) stream)
  (print-unreadable-object (os stream :type t :identity (not *print-pretty*))
    (with-slots (machine name release version) os
    (format stream "~A ~A ~A ~A"
	    name release machine version))))

(defmethod host-os ((host host))
  (get-probed host 'os))

;;  Host probes

(defmethod probe-os-using-uname ((host host) (os t))
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
		       (warn "Unknown OS : ~A" name)))))
      (when class
	(list :os (make-instance class
				 :machine machine
				 :name name
				 :release release
				 :version version))))))

(defmethod probe-hostname ((host host) (os os-unix))
  (cons :hostname (run "hostname")))
