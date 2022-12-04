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

(in-package :cl-user)

(defpackage :cl-unix-cybernetics
  (:use
   :alexandria
   :common-lisp
   :debug
   :parse-number
   :re
   :str)
  (:shadow #:directory #:get-properties)
  (:export
   #:*cl-unix-cybernetics*
   #:*host*
   ;;  Shell
   #:*default-shell-command*
   #:*shell-signal-errors*
   #:shell-error
   #:shell-error-command
   #:shell-error-status
   #:shell-error-out
   #:shell-error-err
   #:sh-quote
   #:sh-parse-integer
   #:ascii-set-graphics-mode
   #:shell
   #:shell-pid
   #:shell-in
   #:shell-out/line
   #:shell-err
   #:shell-err/line
   #:shell-status
   #:shell-close
   #:shell-closed-p
   #:shell-run-command
   #:with-shell
   #:shell-run
   #:make-shell
   ;;  Probe
   #:probe
   #:probe-name
   #:probe-properties
   #:probe-generic-function
   ;;  Resource metaclass
   #:compute-probes
   #:define-resource-class
   #:direct-probes
   #:probe-class
   #:probes-of
   #:resource-class
   ;;  Resource
   #:add-resource
   #:describe-probed
   #:get-resource
   #:make-resource
   #:make-*resources*
   #:probed-properties
   #:probe-all-properties
   #:resource
   #:resource-additional-specs
   #:resource-type
   #:specified-properties
   #:sync
   ;;  Resource container
   #:*parent-resource*
   #:clear-resources
   #:resource-container
   #:with-parent-resource
   ;;  Specification
   #:get-specified
   #:specify
   #:parse-specification
   #:parse-next-specification
   ;;  OS
   #:debian
   #:linux
   #:os
   #:os-bsd
   #:os-darwin
   #:os-freebsd
   #:os-linux
   #:os-linux-debian
   #:os-machine
   #:os-name
   #:os-openbsd
   #:os-release
   #:os-unix
   #:os-version
   #:os-windows
   ;;  Probing resources
   #:clear-probed
   #:find-probe
   #:get-probed
   #:resource-probe-error
   #:resource-probe-not-found
   #:resource-probe-failed
   #:resource-diff
   ;;  Host
   #:current-host
   #:homedir
   #:host
   #:host-connect
   #:host-disconnect
   #:host-os
   #:host-shell
   #:host-run
   #:localhost
   #:run
   #:run-as-root
   #:run-as-root-command
   #:ssh-host
   #:with-connected-host
   #:with-host
   ;;  Unix
   #:define-syntax
   #:directory
   #:egrep
   #:file
   #:grep
   #:group
   #:group<5>
   #:mount
   #:parse-group<5>
   #:parse-passwd<5>
   #:parse-stat<1>
   #:passwd<5>
   #:process
   #:ssh-authorized-key
   #:stat
   #:stat<1>
   #:symlink
   #:+timestamp-offset+
   #:timestamp-to-universal-time
   #:universal-time-to-timestamp
   #:user
   #:with-uptime<1>
   ;; OpenBSD
   #:openbsd-pkg
   ;; helpers
   #:include
   #:read-file
   #:static-file
   #:static-directory
   #:str
   ))

(defpackage :cl-unix-cybernetics-user
  (:use :cl-unix-cybernetics :cl :cl-debug :re)
  (:shadowing-import-from :cl-unix-cybernetics #:directory))

(setf (symbol-function 'cl-unix-cybernetics::directory) #'cl:directory)
