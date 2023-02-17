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

(defmethod run-as-root-command ((host t) (os os-netbsd))
  "sudo ")

(defmethod probe-hostname ((host host) (os os-netbsd))
  (list :hostname (run-1 "hostname -s")))

#+nil
(clear-resources)

#+nil
(describe-probed (resource 'netbsd-pkg "emacs"))

#+nil
(probe-host-packages *host* (host-os *host*))

#+nil
(probe *host* :packages)

#+nil
(map nil #'describe-probed (probe-installed-packages))

#+nil
(run "pkg_info -q | grep emacs-")
