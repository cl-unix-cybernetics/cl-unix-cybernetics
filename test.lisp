;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :cl-user)

(require :adams)

(in-package :adams-user)

;; TEST

(setf (debug-p :shell) t)
(setf (debug-p :sb-shell) nil)

(assert (string= (machine-instance) (run-1 "hostname")))

(adams:clear-resources)
(adams:clear-probed)

(resource 'symlink "/home/dx/test-symlink"
          :owner "dx"
          :group "dx"
          :target "../test")

(resource 'host "ams.kmx.io"
          :user "root"
          :hostname "ams"
          :packages '("emacs:no_x11" "git" "rsync" "sbcl" "texinfo" "texlive_texmf-full")
          (resource 'group "dx"
                    :gid 19256)
          (resource 'user "dx"
                    :uid 19256
                    :gid 19256
                    :home "/home/dx"))

(with-host "ams.kmx.io"
  (sync *host*))

(resource 'host "vu.kmx.io"
          :user "root"
          :hostname "vu"
          :packages '("emacs:no_x11" "git" "rsync" "sbcl" "texinfo" "texlive_texmf-full")
          (resource 'group "dx"
                    :gid 19256
                    :ensure :present)
          (resource 'user "dx"
                    :uid 19256
                    :gid 19256
                    :home "/home/dx"
                    :ensure :present)
          (resource 'group "git"
                    :gid 7000
                    :ensure :present)
          (resource 'user "git"
                    :uid 7000
                    :gid 7000
                    :home "/home/git"
                    :shell "/usr/local/bin/git-auth"
                    :ensure :present))

(with-host "vu.kmx.io"
  (sync *host*))
