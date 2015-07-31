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

(defmethod probe-group-in-/etc/group ((group group) (os os-unix))
  (let ((id (resource-id group)))
    (multiple-value-bind #1=(name passwd gid members)
      (iter (group<5> #1# in (grep (str id) "/etc/group"))
            (when (etypecase id
                    (integer (= id gid))
                    (string (string= id name)))
              (return (values* #1#))))
      (properties* #1#))))

;;  User

(defmethod probe-user-in-/etc/passwd ((user user) (os os-unix))
  (let ((id (resource-id user)))
    (multiple-value-bind #1=(login pass uid gid realname home shell)
      (iter (passwd<5> #1# in
                       (etypecase id
                         (integer (grep (str #\: id #\:) "/etc/passwd"))
                         (string (egrep (str #\^ id #\:) "/etc/passwd"))))
            (when (etypecase id
                    (string (string= id login))
                    (integer (= id uid)))
              (setq home (resource 'directory home)
                    shell (resource 'file shell))
              (return (values* #1#))))
      (properties* #1#))))

(defmethod probe-user-groups-in-/etc/group ((user user) (os os-unix))
  (let* ((id (resource-id user))
	 (user-login (if (stringp id)
			 id
			 (get-probed user :login)))
	 (user-gid (get-probed user :gid))
         (user-group nil)
         (groups (iter (group<5> (name passwd gid members)
                                 in (grep user-login "/etc/group"))
                       (cond ((= user-gid gid)
                              (setq user-group (resource 'group name)))
                             ((find user-login members :test #'string=)
                              (collect (resource 'group name))))))
         (groups (sort groups #'string< :key #'resource-id))
         (groups (if user-group
                     (cons user-group groups)
                     groups)))
    (properties* groups)))

;;  VNode (filesystem node)

(defmethod probe-vnode-using-ls ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (multiple-value-bind #1=(mode links owner group size mtime)
      (iter (ls<1>-lT #.(cons 'name '#1#)
                      in (ls "-ldT" id))
            (when (string= id name)
              (setq mode (mode (mode-permissions mode))
                    owner (resource 'user owner)
                    group (resource 'group group))
              (return (values* #1#))))
      (properties* #1#))))

(defmethod probe-vnode-using-stat ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (multiple-value-bind #1=(dev ino mode links uid gid rdev size
                                 atime mtime ctime blksize blocks flags)
      (iter (stat<1>-r #.(cons 'name '#1#)
                       in (stat "-r" id))
            (when (string= id name)
              (setq mode (mode (mode-permissions mode)))
              (return (values* #1#))))
      (properties* #1#))))

;;  Regular file

#.(cons 'progn
	(iter (for algorithm in *cksum-algorithms*)
	      (for name = (sym 'probe-file-cksum- algorithm))
              (for legacy = (member algorithm *cksum-legacy-algorithms*))
              (for iterator = (if legacy
                                  'cksum<1>-legacy
                                  'cksum<1>))
              (for vars = (if legacy
                              '(sum size name)
                              '(algo name sum)))
              (for cmd = (str "cksum -a " algorithm " "))
              (for match-p = (if legacy
                                 `(string= id name)
                                 `(and (string= ,algorithm algo)
                                       (string= id name))))
              (for var = (sym algorithm))
	      (collect `(defgeneric ,name (file os)))
	      (collect `(defmethod ,name ((file file) (os os-unix))
			  (let* ((id (resource-id file))
                                 (,var (iter (,iterator ,vars
                                                        in (run ,cmd
                                                                (sh-quote id)))
                                             (when ,match-p
                                               (return sum)))))
                            (properties* ,var))))))

(defmethod probe-file-content ((file file) (os os-unix))
  (let* ((size (get-probed file :size))
         (content (when size
                    (if (< size *probe-file-content-size-limit*)
                        (run "cat " (sh-quote (resource-id file)))
                        :file-too-large))))
    (properties* content)))

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
        (iter (mount<8> (dev mp fstype options)
                        in (run "mount | grep "
                                (sh-quote (str id " "))))
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
        (iter (fstab<5> (dev mp fstype options freq passno)
                        in (run "grep "
                                (sh-quote (str id " "))
                                " /etc/fstab"))
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
        (iter (ps<1>-u #1# in (run "ps auxww | grep " (sh-quote id)))
              (print #.(cons 'list '#1#))
              (when (typecase id
                      (integer (= id pid))
                      (string (and (<= (length id) (length cmd))
                                   (string= id cmd :end2 (length id))
                                   (or (= (length id) (length cmd))
                                       (char= #\Space (char cmd (length id)))))))
                (return (values* #1#))))
      (properties* #1#))))

#+nil
(probe-all-properties (resource 'process "svscan"))
