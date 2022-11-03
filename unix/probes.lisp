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

(defmethod probe-group ((group group) (os os-unix))
  (let ((id (resource-id group))
        (ensure :absent))
    (multiple-value-bind #1=(name passwd gid members)
        (with-group<5> #1# (egrep (str "^" id ":") "/etc/group")
          (when (string= id name)
            (setq ensure nil)
            (return (values* #1#))))
      (properties* (ensure . #1#)))))

;;  User

(defmethod probe-user ((user user) (os os-unix))
  (let ((id (resource-id user))
        (ensure :absent))
    (multiple-value-bind #1=(login pass uid gid realname home shell)
      (with-passwd<5> #1#
          (egrep (str #\^ id #\:) "/etc/passwd")
        (when (string= id login)
          (with-parent-resource *host*
            (setq ensure nil
                  home (resource 'directory home)
                  shell (resource 'file shell)))
          (return (values* #1#))))
      (properties* (ensure . #1#)))))

(defmethod probe-user-groups ((user user) (os os-unix))
  (let ((id (resource-id user))
        groups)
    (unless (eq :absent (get-probed user :ensure))
      (let* ((user-login id)
             (user-gid (get-probed user :gid))
             (user-group nil))
        (with-group<5> (name passwd gid members)
            (grep user-login "/etc/group")
          (when name
            (with-parent-resource *host*
              (cond ((= user-gid gid)
                     (setq user-group (resource 'group name)))
                    ((find user-login members :test #'string=)
                     (unless (find name groups :key #'resource-id :test #'string=)
                       (push (resource 'group name) groups)))))
            (setq groups (sort groups #'string< :key #'resource-id)
                  groups (if user-group
                             (cons user-group
                                   (remove (resource-id user-group)
                                           groups :key #'resource-id
                                           :test #'string=))
                             groups))))))
    (properties* groups)))

;;  VNode (filesystem node)

(defmethod probe-vnode-using-ls ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode))
        (ensure :absent))
    (multiple-value-bind #1=(mode links owner group size mtime target)
        (with-ls<1>-lT #.(cons 'name '#1#)
            (ls "-ldT" id)
          (when (string= id (the string name))
            (setq mode (mode (mode-permissions mode))
                  owner (resource 'user owner)
                  group (resource 'group group)
                  size (parse-integer size)
                  ensure :present)
            (return (values* #1#))))
      (properties* (ensure . #1#)))))

(defmethod probe-vnode-using-stat ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode))
        (ensure :absent))
    (multiple-value-bind #1=(dev ino mode links uid gid rdev size
                                 atime mtime ctime blksize blocks flags)
        (with-stat<1>-r (name . #1#) (stat "-r" (sh-quote id))
          (when (and name (string= id (the string name)))
            (setq mode (mode (mode-permissions mode))
                  size (parse-integer size)
                  ensure :present)
            (return (values* #1#))))
        (properties* (ensure . #1#)))))

;;  Regular file

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *cksum-defs* nil))

(eval-when (:compile-toplevel :load-toplevel)
  (defun cksum-defs ()
    (dolist (algorithm *cksum-algorithms*)
      (declare (type symbol algorithm))
      (let* ((name (sym 'probe-file-cksum- algorithm))
             (legacy (member algorithm *cksum-legacy-algorithms*))
             (iterator (if legacy 'with-cksum<1>-legacy
                           'with-cksum<1>))
             (vars (if legacy '(sum size name) '(algo name sum)))
             (cmd (str "cksum -a " algorithm " "))
             (match-p (if legacy
                          `(string= id name)
                          `(and (string= ,algorithm algo)
                                (string= id name))))
             (var (sym algorithm)))
        (push `(defmethod ,name ((file file) (os os-unix))
                 (let* ((id (resource-id file))
                        (,var (,iterator ,vars
                                         (run ,cmd (sh-quote id)))
                          (when ,match-p
                            (return sum))))
                   (properties* ,var)))
              *cksum-defs*)
        (push `(defgeneric ,name (file os)) *cksum-defs*)))
    (setf *cksum-defs* (nreverse *cksum-defs*))
    (push 'progn *cksum-defs*)))

(eval-when (:compile-toplevel :load-toplevel)
  #.(cksum-defs))

(defmethod probe-file-content ((file file) (os os-unix))
  (let* ((size (get-probed file :size))
         (content (when size
                    (if (< size *probe-file-content-size-limit*)
                        (str (run "cat " (sh-quote (resource-id file))))
                        :file-too-large))))
    (properties* content)))

;;  Symlink

(defmethod probe-symlink-target ((symlink symlink) (os os-unix))
  (let ((target (string-trim '(#\Newline)
                             (run-1 "readlink "
                                    (sh-quote (resource-id symlink))))))
    (properties* target)))

;;  Directory

(defmethod probe-directory-content ((dir directory) (os os-unix))
  (let ((content (remove-if (lambda (f)
                              (or (string= "." f)
                                  (string= ".." f)))
                            (run "ls -1a " (resource-id dir)))))
    (properties* content)))

;;  Mounts

(defmethod probe-mount ((m mount) (os os-unix))
  (let ((id (resource-id m)))
    (multiple-value-bind (dev mp fstype options)
        (with-mount<8> (dev mp fstype options)
            (run "mount | grep " (sh-quote (str id " ")))
          (when (or (string= id dev)
                    (string= id mp))
            (return (values dev mp fstype options))))
      (properties :mounted-device dev
                  :mounted-mount-point mp
                  :mounted-fstype fstype
                  :mounted-options options))))

(defmethod probe-fstab ((m mount) (os os-unix))
  (let ((id (resource-id m)))
    (multiple-value-bind (dev mp fstype options freq passno)
        (with-fstab<5> (dev mp fstype options freq passno)
            (run "grep " (sh-quote (str id " ")) " /etc/fstab")
          (when (or (string= id dev)
                    (string= id mp))
            (return (values dev mp fstype options freq passno))))
      (properties :fstab-device dev
                  :fstab-mount-point mp
                  :fstab-fstype fstype
                  :fstab-options options
                  :fstab-freq freq
                  :fstab-passno passno))))

#+nil
(probe-all-properties (resource 'mount "/"))

;;  Processes

(defmethod probe-ps-auxww ((process process) (os os-unix))
  (let ((id (resource-id process)))
    (multiple-value-bind #1=(user pid cpu mem vsz rss tt state start time cmd)
        (with-ps<1>-u #1# (run "ps auxww | grep " (sh-quote id))
          (print #.(cons 'list '#1#))
          (when (and (<= (length id) (length cmd))
                     (string= id cmd :end2 (length id))
                     (or (= (length id) (length cmd))
                         (char= #\Space (char cmd (length id)))))
            (return (values* #1#))))
        (properties* #1#))))

#+nil
(probe-all-properties (resource 'process "svscan"))
