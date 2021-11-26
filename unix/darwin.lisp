;;
;;  adams - system administrator written in Common Lisp
;;
;;  Copyright 2013,2014,2018,2021 Thomas de Grivel <thoxdg@gmail.com>
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

(in-re-readtable)

(defmethod echo-command ((host t) (os os-darwin))
  "echo -E -n ")

(defmethod probe-hostname ((host host) (os os-darwin))
  (list :hostname (run-1 "hostname -s")))

(define-resource-class brew-pkg (pkg)
  ()
  ((probe-brew-pkg :properties (:ensure :flavor :version)))
  ((op-brew-pkg :properties (:ensure :version))))

(defgeneric probe-brew-pkg (resource os))

(defgeneric op-brew-pkg (pkg os &key ensure version))

(defmethod probe-host-packages ((host host) (os os-darwin))
  (list :packages ()))

(defmethod op-host-packages ((host host) (os os-darwin) &key packages)
  nil)

(defmethod op-hostname ((host host) (os os-unix) &key hostname)
  (run-as-root "hostname -s " (sh-quote hostname)))

(defmethod probe-group ((group group) (os os-darwin))
  (let ((id (resource-id group))
        (ensure :absent)
        (gid nil))
    (multiple-value-bind (out status)
        (run "dscl . -read /Groups/" (sh-quote id))
      (when (= 0 status)
        (setq ensure :present)
        (dolist (line out)
          (re-bind "PrimaryGroupID: ([0-9]+)" (n) line
            (setq gid (parse-number n)))))
      (properties* ensure gid))))

#+nil
(probe (resource 'group "dx") :gid)

(defmethod probe-user ((user user) (os os-darwin))
  (let ((id (resource-id user))
        (ensure :absent)
        (uid nil)
        (gid nil))
    (multiple-value-bind (out status)
        (run "dscl . -read /Users/" (sh-quote id))
      (when (= 0 status)
        (setq ensure :present)
        (dolist (line out)
          (re-bind "UniqueID: ([0-9]+)" (n) line
            (setq uid (parse-number n)))
          (re-bind "PrimaryGroupID: ([0-9]+)" (n) line
            (setq gid (parse-number n)))))
      (properties* ensure uid gid))))

#+nil
(probe (resource 'user "root") :gid)

(define-syntax id<1>-tr ((#'parse-integer gid) name)
    #~|([0-9]+)[(]([^)]+)|)

(defmethod probe-user-groups ((user user) (os os-darwin))
  (let ((id (resource-id user))
        (groups nil))
    (unless (eq :absent (get-probed user :ensure))
      (let ((user-gid (get-probed user :gid))
            (user-group nil))
        (with-id<1>-tr (gid name) (run "id " (sh-quote id)
                                       " | tr ' ,=' '\\n'")
          (when gid
            (when (= user-gid gid)
              (setq user-group (resource 'group name)))
            (push (resource 'group name) groups)))
        (setq groups (sort groups #'string< :key #'resource-id)
              groups (if user-group
                         (cons user-group
                               (remove (resource-id user-group)
                                       groups :key #'resource-id
                                              :test #'string=))
                         groups))))
    (properties* groups)))

#+nil
(probe (resource 'user "root") :groups)

(defmethod op-update-group ((group group) (os os-unix) &key ensure gid)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "groupdel")
               ((:present) "groupadd")
               ((nil)      "groupmod"))
             (when gid `("-g" ,(sh-quote gid)))
             (sh-quote (resource-id group)))))

(defmethod op-update-user ((user user) (os os-unix)
                           &key ensure uid gid realname home shell
                             login-class groups)
  (sync-groups)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "userdel")
               ((:present) "dscl -create")
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

#+nil
(clear-resources)

#+nil
(describe-probed (resource 'openbsd-pkg "emacs"))

#+nil
(probe-host-packages *host* (host-os *host*))

#+nil
(probe *host* :packages)

#+nil
(map nil #'describe-probed (probe-installed-packages))

#+nil
(run "pkg_info -q | grep emacs-")
