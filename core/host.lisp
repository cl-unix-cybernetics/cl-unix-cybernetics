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
  (apply #'host-run (current-host) command))

(defun strip-last-newline (string)
  (when (stringp string)
    (let* ((len (length string))
           (len-1 (1- len)))
      (if (< len 1)
          string
          (when (char= #\Newline (char string len-1))
            (subseq string 0 len-1))))))

(defun run-1 (&rest command)
  (strip-last-newline (first (apply #'run command))))

;;  localhost

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
  (or *host* (localhost)))

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

(defun gentoo-release (release)
  (when (search (symbol-name 'gentoo) release :test #'char-equal)
    'gentoo))

(defmethod probe-os-using-uname ((host host) (os t))
  (multiple-value-bind (name hostname release version machine) (uname)
    (declare (ignore hostname))
    (let* ((name (or (linux-name name)
                     name))
           (distrib (or (debian-version version)
                        (gentoo-release release)))
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
  (list :hostname (run-1 "hostname")))

(defmethod probe-boot-time ((host host) (os os-unix))
  (with-uptime<1> (time uptime users load1 load5 load15) (run "uptime")
    (return (list :boot-time (chronicity:parse
                              (str uptime " seconds ago"))))))

(defmethod probe-host-user ((host host) (os os-unix))
  (list :user (run-1 "whoami")))

(defmethod compare-property-values ((host host)
                                    (property (eql :os))
                                    (a os)
                                    (b os))
  (string= (prin1-to-string a)
           (prin1-to-string b)))

(defmethod match-specified-value ((res host)
                                  (property (eql :packages))
                                  (specified list)
                                  (probed list)
                                  os)
  (format t "~&match-specified-value specified ~S~%" specified)
  (format t "~&match-specified-value probed ~S~%" probed)
  (force-output)
  (loop (when (endp specified)
          (return t))
     (let ((pkg (pop specified)))
       (unless (find pkg probed :test #'string=)
         (return nil)))))
