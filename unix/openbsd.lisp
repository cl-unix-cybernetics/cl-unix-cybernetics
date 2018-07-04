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

(in-re-readtable)

(define-resource-class openbsd-pkg (pkg)
  ()
  ((probe-openbsd-pkg :properties (:versions))))

(define-syntax pkg_info<1> (name version)
  #~|\s*([^-\s]+(?:-[^-0-9\s][^-\s]*)*)-([0-9][^-\s]*(?:-[^\s]+)*)|
  "Syntax for pkg_info(1) on OpenBSD"
  (values name (list version)))

(defgeneric probe-openbsd-pkg (resource os))

(defmethod probe-openbsd-pkg ((pkg openbsd-pkg) (os os-openbsd))
  (let ((id (resource-id pkg))
        (ensure :absent))
    (multiple-value-bind (versions)
      (with-pkg_info<1> (name versions)
          (run "pkg_info | egrep " (sh-quote (str "^" id "-")))
        (when (string= id name)
          (setf ensure :installed)
          (return (values versions))))
      (properties* ensure versions))))

(defmethod merge-property-values ((pkg openbsd-pkg)
                                  (property (eql :versions))
                                  (old list)
                                  (new list))
  (sort (remove-duplicates (append old new))
        #'string<))

(defmethod probe-host-packages ((host host) (os os-openbsd))
  (with-host host
    (let ((packages))
      (with-pkg_info<1> (name versions) (run "pkg_info")
        (let ((pkg (resource 'openbsd-pkg name)))
          (add-probed-properties pkg (properties* name versions))
          (push pkg packages)))
      (list :packages (nreverse packages)))))

#+nil
(clear-resources)

#+nil
(describe-probed (resource 'openbsd-pkg "emacs"))

#+nil
(probe-installed-packages)

#+nil
(map nil #'describe-probed (probe-installed-packages))

#+nil
(run "pkg_info -q | grep emacs-")
