;;
;;  adams  -  Remote system administration tools
;;
;;  Copyright 2013 Thomas de Grivel <billitch@gmail.com>
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

(in-package :cl-user)

(defpackage :adams
  (:use :alexandria :cl :debug :iterate)
  (:export
   ;;  Shell
   #:*default-shell-command*
   #:*shell-signal-errors*
   #:shell-error
   #:sh-quote
   #:sh-parse-integer
   #:ascii-set-graphics-mode
   #:shell
   #:shell-pid
   #:shell-in
   #:shell-close
   #:shell-run-command
   #:with-shell
   #:shell-run
   #:make-shell
   ;;  Host
   #:*host*
   #:*localhost*
   #:host
   #:host-connect
   #:host-disconnect
   #:with-connected-host
   #:host-run
   #:ssh-host
   #:with-host
   #:run
   ;;  Resource
   #:resource-class
   #:resource-class-instances
   #:define-resource-class
   #:resource
   #:gather-resource
   ;;  Manifest
   #:manifest
   #:manifest-resources
   #:find-manifest
   #:remove-manifest
   #:with-manifest
   #:define-resource
   ;;  Unix
   #:+timestamp-offset+
   #:timestamp-to-universal-time
   #:universal-time-to-timestamp
   #:grep
   #:egrep
   #:stat
   #:group
   #:define-syntax
   #:parse-group<5>
   #:group<5>
   #:user
   #:parse-passwd<5>
   #:passwd<5>
   #:file
   #:parse-stat<1>
   #:stat<1>))

(defpackage :adams-user
  (:use :adams :cl :cl-debug))
