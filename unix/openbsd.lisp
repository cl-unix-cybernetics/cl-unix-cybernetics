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

(defmethod probe-hostname ((host host) (os os-openbsd))
  (list :hostname (first (run "hostname -s"))))

(define-resource-class openbsd-pkg (pkg)
  ()
  ((probe-openbsd-pkg :properties (:ensure :flavor :version))))

(define-syntax pkg_info<1> (name version flavor installed)
  #~|\s*([^-\s]+(?:-[^-0-9\s][^-\s]+)*)-([0-9][^-\s]*)(?:-([^-\s]+))?( \(installed\))?|
  "Syntax for pkg_info(1) on OpenBSD"
  (values name version flavor (and installed t)))

(defgeneric probe-openbsd-pkg (resource os))

(defmethod probe-openbsd-pkg ((pkg openbsd-pkg) (os os-openbsd))
  (let ((id (resource-id pkg))
        (ensure :absent))
    (re-bind #~|^([^:]+)(?::(.+)$)?| (id-name id-flavor) id
      (multiple-value-bind (version flavor)
          (with-pkg_info<1> (name version flavor installed)
              (run "pkg_info -Q " (sh-quote id-name))
            (when (and (string= id-name name)
                       (or (null id-flavor)
                           (string= id-flavor flavor)))
              (when installed
                (setf ensure :installed))
              (return (values version flavor))))
        (properties* ensure version flavor)))))

(defmethod merge-property-values ((pkg openbsd-pkg)
                                  (property (eql :versions))
                                  (old list)
                                  (new list))
  (sort (remove-duplicates (append old new))
        #'string<))

(defmethod probe-host-packages ((host host) (os os-openbsd))
  (with-host host
    (let ((packages))
      (with-pkg_info<1> (name version flavor) (run "pkg_info")
        (when (and name version)
          (when flavor
            (setq name (str name #\: flavor)))
          (let ((pkg (resource 'openbsd-pkg name)))
            (add-probed-properties pkg (properties* name version flavor))
            (push pkg packages))))
      (list :packages (nreverse packages)))))

#+nil
(clear-resources)

#+nil
(describe-probed (resource 'openbsd-pkg "emacs"))

#+nil
(probe-host-packages *host* (host-os *host*))

#+nil
(probe *host* :packages)

#+nil
(map nil #'describe-probed (probe-installed-packages))

#+nil
(run "pkg_info -q | grep emacs-")
