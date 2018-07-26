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
        (run "apt-get update")
        (run "apt-get install" install-packages)))))

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
