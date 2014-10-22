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

;;  OS

(defclass os ()
  ((machine :initarg :machine
	    :reader os-machine
	    :type string)
   (name :initarg :name
	 :reader os-name
	 :type string)
   (release :initarg :release
	    :reader os-release
	    :type string)
   (version :initarg :version
	    :reader os-version
	    :type string)))

(defmethod print-object ((os os) stream)
  (print-unreadable-object (os stream :type t :identity (not *print-pretty*))
    (with-slots (machine name release version) os
    (format stream "~A ~A ~A ~A"
	    name release machine version))))

;;  Host

(defclass host ()
  ((name :initarg :hostname
	 :reader hostname
	 :type string)
   (shell :initarg :shell
	  :type shell)
   (os :reader host-os
       :type os)))

(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity t)
    (write-string (hostname host) stream)))

;;  Host shell

(defgeneric host-connect (host))
(defgeneric host-disconnect (host))
(defgeneric host-shell (host))
(defgeneric (setf host-shell) (shell host))

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
    `(let ((,g!host (make-instance 'ssh-host :hostname ,hostname)))
       (unwind-protect (let ((,var ,g!host)) ,@body)
	 (host-disconnect ,g!host)))))

(defgeneric host-run (host command &rest format-args))

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
				    :hostname "localhost")))

(defmethod host-connect ((host (eql *localhost*)))
  (setf (host-shell host) (make-shell)))

;;  SSH host

(defclass ssh-host (host) ())

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
