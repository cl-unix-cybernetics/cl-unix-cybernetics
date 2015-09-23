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

;;  Group

(define-resource-class group () ()
  ((probe-group-in-/etc/group :properties (:ensure :name :passwd :gid :members)))
  ((op-update-group :properties (:ensure :gid))))

(defgeneric probe-group-in-/etc/group (resource os))
(defgeneric op-update-group (resource os &key ensure gid))

;;  User

(define-resource-class user ()
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
  ((probe-vnode-using-ls :properties (:mode :links :owner :group :size :mtime))
   (probe-vnode-using-stat :properties (:dev :ino :mode :links :uid :gid :rdev
                                        :size :atime :mtime :ctime :blksize
                                        :blocks :flags)))
  ((op-chown :properties (:uid :gid :owner :group))))

(defgeneric probe-vnode-using-ls (resource os))
(defgeneric probe-vnode-using-stat (resource os))
(defgeneric op-chown (resource os &key uid gid owner group &allow-other-keys))

;;  File

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *cksum-legacy-algorithms*
    '(:cksum))

  (defvar *cksum-algorithms*
    `(:md5 :rmd160 :sha1 :sha224 :sha256 :sha384 :sha512
           ,@*cksum-legacy-algorithms*)))

(defvar *probe-file-content-size-limit* 8192)

(define-resource-class file (vnode)
  ()
  ((probe-file-content :properties (:content))
   . #.(iter (for algorithm in *cksum-algorithms*)
	     (collect `(,(sym 'probe-file-cksum- algorithm)
			 :properties (,algorithm))))))

(defgeneric probe-file-content (resource os))

;;  Directory

(define-resource-class directory (vnode)
  ()
  ((probe-directory-content :properties (:content))))

(defgeneric probe-directory-content (resource os))

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

(defgeneric probe-installed-packages% (host os))
