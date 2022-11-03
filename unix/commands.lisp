;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(in-re-readtable)

(defun uname ()
  (re-bind #~"^(\S+) (\S+) (\S+) (.+) (\S+)$"
      (os-name node-name os-release os-version machine)
    (run-1 "uname -a")))

(defun grep_ (pattern &rest files)
  (join-str " " "grep" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun grep (pattern &rest files)
  (run (apply #'grep_ pattern files)))

(defun egrep_ (pattern &rest files)
  (join-str " " "egrep" (sh-quote pattern) (mapcar #'sh-quote files)))

(defun egrep (pattern &rest files)
  (run (apply #'egrep_ pattern files)))

(defun stat_ (options &rest files)
  (join-str " " "stat" options (mapcar #'sh-quote files)))

(defun stat (options &rest files)
  (run (apply #'stat_ options files)))

(defun ls_ (options &rest files)
  (join-str " " "ls" options (mapcar #'sh-quote files)))

(defun ls (options &rest files)
  (run (apply #'ls_ options files)))

(defun sudo_ (&rest command)
  (join-str " " "sudo" command))

(defun sudo (&rest command)
  (run (apply #'sudo_ command)))

(defun doas_ (&rest command)
  (join-str " " "doas" command))

(defun doas (&rest command)
  (run (apply #'doas_ command)))
