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

;;  Group

(define-resource-class group () ()
  ((probe-group-in-/etc/group :properties (:ensure :name :passwd :gid :members)))
  ((op-update-group :properties (:ensure :gid))))

(defgeneric probe-group-in-/etc/group (resource os))
(defgeneric op-update-group (resource os &key ensure gid))

;;  User

(define-resource-class user (resource-container)
  ()
  ((probe-user-in-/etc/passwd :properties (:ensure :login :uid :gid :realname
                                                   :home :shell))
   (probe-user-groups-in-/etc/group :properties (:groups)))
  ((op-update-user :properties (:ensure :uid :gid :realname :home :shell
                                        :login-class :groups))))

(defgeneric probe-user-in-/etc/passwd (resource os))
(defgeneric probe-user-groups-in-/etc/group (resource os))
(defgeneric op-update-user (resource os &key ensure uid gid realname home shell
                                          login-class
                                          groups))

(defmethod resource-before-p ((r1 group) (r2 user))
  t)

;;  Filesystem virtual node

(define-resource-class vnode ()
  ()
  ((probe-vnode-using-ls :properties (:ensure :group :links :mode :mtime
                                              :owner :size))
   (probe-vnode-using-stat :properties (:atime :blocks :blksize :ctime
                                               :dev :ensure :flags :gid
                                               :ino :links :mode :mtime
                                               :rdev :size :uid)))
  ((op-chmod :properties (:mode))
   (op-chown :properties (:uid :gid :owner :group))))

(defgeneric probe-vnode-using-ls (resource os))
(defgeneric probe-vnode-using-stat (resource os))
(defgeneric op-chmod (resource os &key mode &allow-other-keys))
(defgeneric op-chown (resource os &key uid gid owner group &allow-other-keys))

;;  File

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *cksum-legacy-algorithms*
    '(:cksum))

  (defvar *cksum-algorithms*
    `(:md5 :rmd160 :sha1 :sha224 :sha256 :sha384 :sha512
           ,@*cksum-legacy-algorithms*)))

(defvar *probe-file-content-size-limit* (* 1024 1024))

(define-resource-class file (vnode)
  ()
  ((probe-file-content :properties (:content))
   . #.(let ((algorithms))
         (dolist (algorithm *cksum-algorithms*)
           (push `(,(sym 'probe-file-cksum- algorithm)
                    :properties (,algorithm))
                 algorithms))
         (nreverse algorithms)))
  ((op-file-ensure :properties (:ensure))
   (op-file-content :properties (:content)))
  ((:op-properties (:content :ensure :mode :uid :gid :owner :group))))

(defgeneric probe-file-content (resource os))
(defgeneric op-file-ensure (resource os &key ensure))
(defgeneric op-file-content (resource os &key content))

;;  Directory

(define-resource-class directory (vnode)
  ()
  ((probe-directory-content :properties (:content)))
  ((op-directory-ensure :properties (:ensure)))
  ((:op-properties (:ensure :mode :uid :gid :owner :group))))

(defgeneric probe-directory-content (resource os))
(defgeneric op-directory-ensure (resource os &key ensure))

;;  Mounted filesystems

(define-resource-class mount ()
  ()
  ((probe-mount :properties (:mounted-device
                             :mounted-mount-point
                             :mounted-fstype
                             :mounted-options))
   (probe-fstab :properties (:fstab-device
                             :fstab-mount-point
                             :fstab-fstype
                             :fstab-options
                             :fstab-freq
                             :fstab-passno))))

(defgeneric probe-mount (resource os))
(defgeneric probe-fstab (resource os))

;;  Process

(define-resource-class process ()
  ()
  ((probe-ps-auxww :properties (:user :pid :cpu :mem :vsz :rss :tt :state
                                :start :time :cmd))))

(defmethod probe-ps-auxww (process os))

;;  Software package

(define-resource-class pkg)

(defgeneric probe-installed-packages (host os))
