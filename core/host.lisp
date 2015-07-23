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

(defun run (command &rest format-args)
  (if (and (boundp '*host*)
           (symbol-value '*host*))
      (apply #'host-run *host* command format-args)
      (with-shell (shell)
        (apply #'shell-run shell command format-args))))

;;  localhost

(assert (string= (machine-instance) (first (run "hostname"))))

(defun local-hostname ()
  (machine-instance))

(defun localhost ()
  (let ((id (local-hostname)))
    (or #1=(get-resource 'host id)
        (setf #1# (make-instance 'host :id id)))))

(eval-when (:load-toplevel)
  (setq *host* (localhost)))

(defun host-connect (host)
  (let ((id (resource-id host)))
    (cond
      ((string-equal (local-hostname) id)
       (setf (host-shell host) (make-shell)))
      (:otherwise
       (setf (host-shell host) (make-shell "/usr/bin/ssh" id))))))

(defun current-host ()
  (or (when (boundp '*host*)
        (symbol-value '*host*))
      (localhost)))

;;  Host

(defun hostname (host)
  (resource-id (the host host)))

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

(defun host (host)
  (etypecase host
    (host host)
    (string (if (or (string-equal (local-hostname) host)
                    (string-equal "localhost" host)
                    (string= "127.0.0.1" host))
                (localhost)
                (resource 'ssh-host host)))))

(defmethod host-run ((host host) (command string) &rest format-args)
  (let ((shell (host-shell host)))
    (when (shell-closed-p shell)
      (setq shell (host-connect host)))
    (apply #'shell-run shell command format-args)))

(defmacro with-connected-host ((var hostname) &body body)
  (let ((g!host (gensym "HOST-")))
    `(let ((,g!host (make-instance 'ssh-host :id ,hostname)))
       (unwind-protect (let ((,var ,g!host)) ,@body)
         (host-disconnect ,g!host)))))

(defmethod host-run ((hostname string) command &rest format-args)
  (with-connected-host (host hostname)
    (apply #'host-run host command format-args)))

;;  With host

(defmacro with-host (host &body body)
  `(let ((*host* (host ,host)))
     (with-parent-resource *host*
       ,@body)))

;;  OS

(defmethod print-object ((os os) stream)
  (print-unreadable-object (os stream :type t :identity (not *print-pretty*))
    (with-slots (machine name release version) os
    (format stream "~A ~A ~A ~A"
	    name release machine version))))

(defmethod host-os ((host host))
  (get-probed host :os))

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

(defmethod probe-boot-time ((host host) (os os-unix))
  (iter (uptime<1> (time uptime users load1 load5 load15) in (run "uptime"))
        (return (list :boot-time (chronicity:parse
                                  (str uptime " seconds ago"))))))
