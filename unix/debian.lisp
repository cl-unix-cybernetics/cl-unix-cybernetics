;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(in-re-readtable)

(define-resource-class debian-pkg (pkg)
  ()
  ((probe-debian-pkg :properties (:arch :ensure :release :tags
                                        :versions))))

(define-syntax apt<8>-list (name release version arch tags)
  #~|([^/\s]+)(?:/([^\s]*))?\s+([^\s]+)\s+([^\s]+)(?:\s+\[([^\]]+)\])?|
  "Syntax for apt(8) list on Linux Debian"
  (values name release version arch (cl-ppcre:split "," tags)))

(defmethod probe-debian-pkg ((pkg debian-pkg) (os os-linux-debian))
  (let ((id (resource-id pkg))
        (ensure :absent))
    (multiple-value-bind #1=(release version arch tags)
        (with-apt<8>-list (name . #1#)
            (run "apt list " (sh-quote id))
          (when (and name (string= id (the string name)))
            (when (find "installed" (the list tags))
              (setf ensure :installed))
            (return (values* #1#))))
      (properties* (ensure . #1#)))))

(defmethod probe-host-packages ((host host) (os os-linux-debian))
  (with-host host
    (let ((packages))
      (with-apt<8>-list (name release version arch tags)
          (run "apt list | grep installed")
        (let ((pkg (resource 'debian-pkg name))
              (ensure (if (find "installed" (the list tags))
                          :installed
                          :absent)))
          (add-probed-properties pkg (properties* name release version
                                                  arch tags ensure))
          (push pkg packages)))
      (list :packages (nreverse packages)))))

(defmethod op-host-packages ((host host) (os os-linux-debian) &key packages)
  (with-host host
    (let ((install-packages
           (with-output-to-string (out)
             (dolist (id packages)
               (let ((pkg (resource 'debian-pkg id :ensure :installed)))
                 (unless (eq :installed (get-probed pkg :ensure))
                   (write-str out " " (sh-quote id))))))))
      (unless (string= "" install-packages)
        (run "apt-get -qy update")
        (run "apt-get -qy install" install-packages)))))

(defmethod probe-host-locale ((host host) (os os-linux-debian))
  (with-host host
    (let ((lang (with-sh-var (var value) (run "cat /etc/default/locale")
                  (when (and var value (string= "LANG" (the string var)))
                    (return value)))))
      (list :locale lang))))

(defmethod op-host-locale ((host host) (os os-linux-debian) &key locale)
  (let ((sh-locale (sh-quote locale)))
    (run "echo LANG=" sh-locale " > /etc/default/locale")
    (run "export LANG=" sh-locale)))

(defmethod op-hostname ((host host) (os os-linux-debian) &key hostname)
  (let ((sh-hostname (sh-quote hostname)))
    (run "echo " sh-hostname " > /etc/hostname")
    (run "hostname " sh-hostname)))
