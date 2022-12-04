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

;;  Echo

(defgeneric echo-command (host os))

(defmethod echo-command ((host t) (os t))
  "echo -n ")

(defun echo_ (&rest parts)
  (let* ((host (current-host))
         (cmd (echo-command host (host-os host))))
    (str cmd (sh-quote parts))))

(defun echo (&rest parts)
  (run (echo_ parts)))

;;  Run as root

(defgeneric run-as-root-command (host os))

(defmethod run-as-root-command ((host t) (os os-unix))
  "sudo ")

(defun run-as-root (&rest command)
  (let* ((host (current-host))
         (prefix (unless (equal "root" (get-probed host :user))
                   (run-as-root-command host (get-probed host :os)))))
    (apply #'run prefix command)))

;;  Host operations

(defmethod op-hostname ((host host) (os os-unix) &key hostname)
  (run-as-root "hostname -s " (sh-quote hostname)))

;;  Group

(defmethod op-update-group ((group group) (os os-unix) &key ensure gid)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "groupdel")
               ((:present) "groupadd")
               ((nil)      "groupmod"))
             (when gid `("-g" ,(sh-quote gid)))
             (sh-quote (resource-id group)))))

;;  User

(defun sync-groups ()
  (do-resources (res) (current-host)
    (when (typep res 'group)
      (sync res))))

(defmethod op-update-user ((user user) (os os-unix)
                           &key ensure uid gid realname home shell
                             login-class groups)
  (sync-groups)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "userdel")
               ((:present) `("useradd"
                             ,(unless (eq :present
                                          (get-probed
                                           (resource 'directory (homedir
                                                                 user))
                                           :ensure))
                                "-m")))
               ((nil)      "usermod"))
             (when realname `("-c" ,(sh-quote realname)))
             (when home `("-d" ,(sh-quote home)))
             (when gid `("-g" ,(sh-quote gid)))
             (when login-class `("-L" ,(sh-quote login-class)))
             (when groups `("-G" ,(join-str "," (mapcar #'sh-quote
                                                        groups))))
             (when shell `("-s" ,(sh-quote shell)))
             (when uid `("-u" ,(sh-quote uid)))
             (sh-quote (resource-id user)))))

;;  VNode

(defgeneric vnode-owner (res))

(defmethod vnode-owner ((res vnode))
  (let ((owner-spec (get-specified res :owner)))
    (when owner-spec
      (resource 'user owner-spec))))
  
(defgeneric vnode-group (res))

(defmethod vnode-group ((res vnode))
  (let ((group-spec (get-specified res :group)))
    (when group-spec
      (resource 'user group-spec))))
  
(defgeneric sync-owner-and-group (res))

(defmethod sync-owner-and-group ((res vnode))
  (let ((owner (vnode-owner res))
        (group (vnode-group res)))
    (when group
      (sync group))
    (when owner
      (sync owner))))

(defmethod op-chown ((res vnode) (os os-unix) &key uid gid owner group
                                                &allow-other-keys)
  (when (stringp owner)
    (setq owner (resource 'user owner)))
  (when (stringp group)
    (setq group (resource 'group group)))
  (when (and uid owner)
    (assert (= uid (get-probed owner :uid))))
  (when (and gid group)
    (assert (= gid (get-probed group :gid))))
  (when group
    (sync group))
  (when owner
    (sync owner))
  (let ((u (or (when owner (resource-id owner))
               uid))
        (g (or (when group (resource-id group))
               gid)))
    (run "chown "
         (sh-quote u)
         (when g `(":" ,(sh-quote g)))
         " "
         (sh-quote (resource-id res)))))

(defmethod op-chmod ((res vnode) (os os-unix) &key mode
                                                &allow-other-keys)
  (sync-owner-and-group res)
  (run "chmod " (octal (mode-permissions mode)) " "
       (sh-quote (resource-id res))))

;;  File

(defun path-parent-directory (&rest path-parts)
  (let* ((path (the string (str path-parts)))
         (sep (position #\/ path
                        :from-end t
                        :end (1- (length path))
                        :test #'char=)))
    (if sep
        (subseq path 0 sep)
        "/")))

(defgeneric parent-directory (x))

(defmethod parent-directory ((res vnode))
  (let* ((path (resource-id res))
         (parent-path (path-parent-directory path)))
    (resource 'directory parent-path)))

(defmethod op-file-ensure ((res file) (os os-unix) &key ensure)
  (sync-owner-and-group res)
  (sync (parent-directory res))
  (let* ((id (resource-id res))
         (sh-id (sh-quote id)))
    (ecase ensure
      ((:absent) (run "rm " sh-id))
      ((:present) (run "touch " sh-id))
      ((nil)))))

(defmethod op-file-content ((res file) (os os-unix) &key content)
  (sync-owner-and-group res)
  (sync (parent-directory res))
  (let ((id (resource-id res)))
    (run (echo_ content) " > " (sh-quote id))
    (when-let (after (get-specified res :after))
      (funcall (the function after) res os))
    (clear-probed res)))

;;  Symlink

(defmethod op-symlink-ensure ((res symlink) (os os-unix)
                              &key ensure)
  (sync-owner-and-group res)
  (sync (parent-directory res))
  (let* ((id (resource-id res))
         (sh-id (sh-quote id))
         (target (get-specified res :target))
         (sh-target (sh-quote target)))
    (ecase ensure
      ((:absent) (run "rm " sh-id))
      ((:present) (run "ln -s " sh-target " " sh-id))
      ((nil)))))

(defmethod op-symlink-target ((res symlink) (os os-unix)
                              &key target)
  (let* ((id (resource-id res))
         (sh-id (sh-quote id))
         (sh-target (sh-quote target)))
    (run "ln -sf " sh-target " " sh-id)))

;;  Directory

(defmethod op-directory-ensure ((res directory) (os os-unix)
                                &key ensure)
  (sync-owner-and-group res)
  (sync (parent-directory res))
  (let* ((id (resource-id res))
         (sh-id (sh-quote id)))
    (ecase ensure
      ((:absent) (run "rmdir " sh-id))
      ((:present) (run "mkdir " sh-id))
      ((nil)))))
