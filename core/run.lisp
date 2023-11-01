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

(defun run (&rest command)
  "Run a command at the current host. COMMAND is assembled using STR."
  (apply #'host-run (current-host) command))

(defun run! (&rest command)
  "Run a command at the current host. COMMAND is assembled using STR.
An error is raised if the command did not complete successfully."
  (multiple-value-bind (out ret err) (apply #'run command)
    (if (zerop ret)
        (values out ret err)
        (error "The command ~S failed with error code ~D.
Output : ~S
Error output : ~S"
               (str command) ret out err))))

;;  Run as root

(defgeneric run-as-root-command (host os))

(defmethod run-as-root-command ((host t) (os os-unix))
  "sudo ")

(defun run-as-root (&rest command)
  "Run a command at the current host as the root user (dangerous).
COMMAND is assembled using STR."
  (let* ((host (current-host))
         (prefix (unless (equal "root" (get-probed host :user))
                   (run-as-root-command host (get-probed host :os)))))
    (apply #'run prefix command)))

(defun run-as-root! (&rest command)
  "Run a command at the current host as the root user (dangerous).
COMMAND is assembled using STR.
An error is raised if the command did not complete successfully."
  (multiple-value-bind (out ret err) (apply #'run-as-root command)
    (if (zerop ret)
        (values out ret err)
        (error "The root command (dangerous) ~S failed with error code ~D.
Output : ~S
Error output : ~S"
               (str command) ret out err))))
