
(in-package :cl-user)

(require :adams)

(use-package :adams)

;; TEST

#+nil
(untrace shell-status)

(setf *debug* '(shell))

(with-shell (shell)
  (run-command "hostname && false" shell)
  (run-command "pwd" shell)
  (run-command "ls" shell)
  (run-command "exit" shell))
