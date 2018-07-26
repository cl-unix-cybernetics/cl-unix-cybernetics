;;
;;  adams - system administrator written in Common Lisp
;;
;;  Copyright 2013,2014,2018 Thomas de Grivel <thoxdg@gmail.com>
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

(defun run (&rest command)
  "Run a command at the current host. COMMAND is assembled using STR."
  (if (and (boundp '*host*)
           (symbol-value '*host*))
      (apply #'host-run *host* command)
      (with-shell (shell)
        (apply #'shell-run shell command))))

;;  localhost

(assert (string= (machine-instance) (first (run "hostname"))))

(defun local-hostname ()
  (machine-instance))

(defun localhost ()
  (let ((id (local-hostname)))
    (or #1=(get-resource 'host id)
        (setf #1# (make-instance 'host :id id)))))

(unless (boundp '*host*)
  (setf *host* (localhost)))

(defun host-user (host)
  (get-specified host :user))

(defun host-connect (host)
  (let* ((id (resource-id host))
         (locale (get-specified host :locale))
         (shell (cond ((string-equal (local-hostname) id)
                       (make-shell "/bin/sh"))
                      (:otherwise
                       (let ((user (host-user host)))
                         (apply #'make-shell
                                `("/usr/bin/ssh"
                                  ,@(when user `("-l" ,user))
                                  ,id "/bin/sh")))))))
    (when locale
      (shell-run shell "export LANG=" locale))
    (setf (host-shell host) shell)
    shell))

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

(defun host (&optional host)
  (etypecase host
    (null (localhost))
    (host host)
    (string (if (or (string-equal (local-hostname) host)
                    (string-equal "localhost" host)
                    (string= "127.0.0.1" host))
                (localhost)
                (resource 'host host)))))

(defmethod host-run ((host host) &rest command)
  (let ((shell (host-shell host)))
    (loop
       (if (shell-closed-p shell)
           (setq shell (host-connect host))
           (return)))
    (apply #'shell-run shell command)))

(defmethod host-run ((hostname string) &rest command)
  (let ((host (host hostname)))
    (apply #'host-run host command)))

;;  With host

(defmacro with-host (host &body body)
  `(let ((*host* (host ,host)))
     (with-parent-resource *host*
       ,@body)))

;;  OS

(defmethod host-os ((host host))
  (get-probed host :os))

;;  Host probes

(defmethod describe-probed% ((host host) (out (eql :form)))
  (with-host host
    (call-next-method)))

(defun linux-name (name)
  (when (string-equal (symbol-name 'linux) name)
    'linux))

(defun debian-version (version)
  (when (search (symbol-name 'debian) version :test #'char-equal)
    'debian))

(defmethod probe-os-using-uname ((host host) (os t))
  (multiple-value-bind (name hostname release version machine) (uname)
    (declare (ignore hostname))
    (let* ((name (or (linux-name name)
                     name))
           (distrib (or (debian-version version)))
           (class (flet ((try (&rest parts)
                           (when-let ((s (find-symbol (string-upcase (str 'os- parts))
                                                      *package*)))
                             (ignore-errors (find-class s)))))
                    (or (try name '- release '- machine '- distrib)
                        (try name '- release '- machine '- version)
                        (try name '- release '- machine)
                        (try name '- release '- distrib)
                        (try name '- release '- version)
                        (try name '- release)
                        (try name '- machine '- distrib)
                        (try name '- machine '- version)
                        (try name '- machine)
                        (try name '- distrib)
                        (try name '- version)
                        (try name)
                        (warn "Unknown OS : ~A" name)))))
      (when class
        (let ((plist (list :machine machine
                           :name name
                           :release release
                           :version version)))
          (when distrib
            (setf plist (list* :distrib distrib plist)))
          (let ((os (apply #'make-instance class plist)))
            (list :os os)))))))

(defmethod probe-hostname ((host host) (os os-unix))
  (list :hostname (first (run "hostname"))))

(defmethod probe-boot-time ((host host) (os os-unix))
  (with-uptime<1> (time uptime users load1 load5 load15) (run "uptime")
    (return (list :boot-time (chronicity:parse
                              (str uptime " seconds ago"))))))

(defmethod probe-host-user ((host host) (os os-unix))
  (list :user (first (run "whoami"))))

(defmethod compare-property-values ((host host)
                                    (property (eql :os))
                                    (a os)
                                    (b os))
  (string= (prin1-to-string a)
           (prin1-to-string b)))
