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
    (iter (group<5> (name passwd gid members) in (grep (str id) "/etc/group"))
	  (when (etypecase id
		  (integer (= id gid))
		  (string (string= id name)))
	    (return (list :name name
			  :passwd passwd
			  :gid gid
			  :members members))))))

;;  User

(defmethod probe-user-in-/etc/passwd ((user user) (os os-unix))
  (let ((id (resource-id user)))
    (iter (passwd<5> (login pass uid gid realname home shell)
		     in (etypecase id
			  (integer (grep (str #\: id #\:) "/etc/passwd"))
			  (string (egrep (str #\^ id #\:) "/etc/passwd"))))
	  (when (etypecase id
		  (string (string= id login))
		  (integer (= id uid)))
	    (return (list :login login :uid uid :gid gid
			  :realname realname :home home :shell shell))))))

(defmethod probe-user-groups-in-/etc/group ((user user) (os os-unix))
  (let* ((id (resource-id user))
	 (user-login (if (stringp id)
			 id
			 (get-probed user :login)))
	 (user-gid (get-probed user :gid)))
    (iter (group<5> (name passwd gid members) in (grep user-login
						       "/etc/group"))
	  (with user-group = nil)
	  (cond ((= user-gid gid) (setq user-group name))
		((find user-login members :test #'string=) (collect name into groups)))
	  (finally (let ((groups (sort groups #'string<)))
		     (return (list :groups (if user-group
					       (cons user-group groups)
					       groups))))))))

;;  VNode (filesystem node)

(defmethod probe-vnode-using-ls ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (iter (ls<1>-lT (name mode links owner group size mtime)
		    in (ls "-ldT" id))
	  (when (string= id name)
	    (return (list :mode mode
			  :links links
			  :owner owner
			  :group group
			  :size size
			  :mtime mtime))))))

(defmethod probe-vnode-using-stat ((vnode vnode) (os os-unix))
  (let ((id (resource-id vnode)))
    (iter (stat<1>-r (name dev ino mode links uid gid rdev size
			   atime mtime ctime blksize blocks flags)
		     in (stat "-r" id))
	  (when (string= id name)
	    (return (list :dev dev
			  :ino ino
			  :mode (mode-string mode)
			  :links links
			  :uid uid
			  :gid gid
			  :rdev rdev
			  :size size
			  :atime atime
			  :mtime mtime
			  :ctime ctime
			  :blksize blksize
			  :blocks blocks
			  :flags flags))))))

;;  Regular file

#.(cons 'progn
	(iter (for algorithm in *cksum-algorithms*)
              (for legacy = (member algorithm *cksum-legacy-algorithms*))
	      (for name = (sym 'probe-file-cksum- algorithm))
	      (collect `(defgeneric ,name (file os)))
	      (collect `(defmethod ,name ((file file) (os os-unix))
			  (let ((id (resource-id file)))
			    (iter (,(if legacy 'cksum<1>-legacy 'cksum<1>)
                                    ,(if legacy
                                         '(sum size name)
                                         '(algo name sum))
                                    in (run ,(str "cksum -a " algorithm " ~A")
                                            (sh-quote id)))
				  (when ,(if legacy
                                             `(string= id name)
                                             `(and (string= ,algorithm algo)
                                                   (string= id name)))
				    (return (list ,algorithm sum)))))))))

(defgeneric probe-file-content (file os))

(defvar *probe-file-content-size-limit* 8192)

(defmethod probe-file-content ((file file) (os os-unix))
  (when (< (get-probed file :size) *probe-file-content-size-limit*)
    (list :content (run "cat ~A" (sh-quote (resource-id file))))))
