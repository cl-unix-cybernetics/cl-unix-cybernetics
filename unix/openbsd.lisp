;; cl-unix-cybernetics
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(in-package :cl-unix-cybernetics)

(in-re-readtable)

(defmethod echo-command ((host t) (os os-openbsd))
  "echo -E -n ")

(defmethod run-as-root-command ((host t) (os os-openbsd))
  "doas ")

(defmethod probe-hostname ((host host) (os os-openbsd))
  (list :hostname (run-1 "hostname -s")))

(define-resource-class openbsd-pkg (pkg)
  ()
  ((probe-openbsd-pkg :properties (:ensure :flavor :version)))
  ((op-openbsd-pkg :properties (:ensure :version))))

(define-syntax pkg_info<1> (name version flavor installed)
  #~|\s*([^-\s]+(?:-[^-0-9\s][^-\s]+)*)-([0-9][^-\s]*)(?:-([^-\s]+))?( \(installed\))?|
  "Syntax for pkg_info(1) on OpenBSD"
  (values name version flavor (and installed t)))

(define-syntax openbsd-pkg-id (name version flavor)
  #~|^(.*?)(?:-([0-9].*?))?(?::(.+))?$|
  "Syntax for openbsd-pkg id")

(defgeneric probe-openbsd-pkg (resource os))

(defmethod probe-openbsd-pkg ((pkg openbsd-pkg) (os os-openbsd))
  (let ((id (resource-id pkg))
        (ensure :absent))
    (with-openbsd-pkg-id (id-name id-version id-flavor) (list id)
      (format t "~&id-name ~S id-flavor ~S~%" id-name id-flavor)
      (multiple-value-bind (version flavor)
          (with-pkg_info<1> (name version flavor installed)
              (run "pkg_info | egrep " (sh-quote (str "^" id-name)))
            (format t "~&name ~S version ~S flavor ~S installed ~S~%" name version flavor installed)
            (when (and (string= id-name name)
                       (or (and (null id-flavor) (null flavor))
                           (and id-flavor flavor
                                (string= id-flavor flavor)))
                       (or (null id-version)
                           (string= id-version version)))
              (setf ensure :installed)
              (return (values version flavor))))
        (return (properties* ensure version flavor))))))

(defmethod match-specified-value ((res host)
                                  (property (eql :packages))
                                  (specified list)
                                  (probed list)
                                  (os os-openbsd))
  (dolist (pkg-id specified)
    (unless (find pkg-id probed :test #'string=)
      (return-from match-specified-value nil)))
  t)

(defmethod op-openbsd-pkg ((pkg openbsd-pkg) (os os-openbsd) &key ensure version)
  (with-openbsd-pkg-id (id-name id-version id-flavor) (list (resource-id pkg))
    (setq version
	  (or version
	      id-version
	      (progn (probe pkg :version)
		     (get-probed pkg :version))))
    (let ((pkg-string (str id-name
			   (when version
                             `(#\- ,version))
                           (when id-flavor
                             `(#\- ,id-flavor)))))
      (cond
        ((eq ensure :absent)
         (run "pkg_delete " (sh-quote pkg-string)))
        ((eq ensure :installed)
         (run "pkg_add " (sh-quote pkg-string)))
        (t
         (error "unknown ensure value: ~S" ensure))))))

(defmethod probe-host-packages ((host host) (os os-openbsd))
  (with-host host
    (let ((packages)
          (ensure :installed))
      (with-pkg_info<1> (name version flavor installed) (run "pkg_info")
        (when (and name version)
          (when flavor
            (setq name (str name #\: flavor)))
          (let ((pkg (resource 'openbsd-pkg name)))
            (add-probed-properties pkg (properties* name version flavor ensure))
            (push pkg packages))))
      (list :packages (nreverse packages)))))

(defmethod op-host-packages ((host host) (os os-openbsd) &key packages)
  (with-host host
    (dolist (pkg-id packages)
      (with-openbsd-pkg-id (name version flavor) (list pkg-id)
	(let ((pkg (resource 'openbsd-pkg pkg-id
                             :ensure :installed)))
          (when version
            (resource 'openbsd-pkg pkg-id
                      :version version))
          (when flavor
            (resource 'openbsd-pkg pkg-id
                      :flavor flavor))
          (sync pkg))))))

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
