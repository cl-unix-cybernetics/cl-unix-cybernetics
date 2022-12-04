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

(in-re-readtable)

(defmethod echo-command ((host t) (os os-darwin))
  "printf %s ")

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
  (let* ((id (resource-id user))
         (sh-id (sh-quote id))
         (ensure :absent)
         uid
         gid
         shell
         home)
    (multiple-value-bind (out status)
        (run "dscl . -read /Users/" sh-id)
      (when (= 0 status)
        (setq ensure :present)
        (dolist (line out)
          (re-bind #~|^UniqueID: ([0-9]+)| (n) line
            (setq uid (parse-number n)))
          (re-bind #~|^PrimaryGroupID: ([0-9]+)| (n) line
            (setq gid (parse-number n)))
          (re-bind #~|^UserShell: (/[^ \n]+)| (s) line
            (setq shell s))
          (re-bind #~|^NFSHomeDirectory: (/[^ \n]+)| (h) line
            (setq home h)))))
    (let ((realname (string-trim
                     '(#\Space #\Newline)
                     (second (run "dscl . -read /Users/" sh-id
                                  " RealName")))))
      (properties* ensure uid gid shell home realname))))

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
                         groups)
              groups (remove-if (lambda (x)
                                  (find (resource-id x)
                                        '("_lpoperator"
                                          "com.apple.access_ssh"
                                          "com.apple.sharepoint.group.1"
                                          "com.apple.sharepoint.group.2"
                                          "everyone"
                                          "localaccounts")
                                        :test #'string=))
                                groups))))
    (properties* groups)))

#+nil
(probe (resource 'user "root") :groups)

(defmethod op-update-group ((group group) (os os-darwin) &key ensure gid)
  (let ((id (resource-id group)))
    (run-as-root
     (join-str
      " "
      "dscl ." 
      (ecase ensure
        ((:absent)  "-delete")
        ((:present) "-create")
        ((nil)      "-change"))
      (str "/Groups/" (sh-quote id))
      (when gid
        `("PrimaryGroupID"
          ,(unless ensure
             (sh-quote (get-probed group :gid)))
          ,(sh-quote gid)))))))

(defmethod op-update-user ((user user) (os os-darwin)
                           &key ensure uid gid realname home shell
                             login-class groups)
  (declare (ignore login-class))
  (sync-groups)
  (let* ((id (resource-id user))
         (sh-id (sh-quote id)))
    (when ensure
      (run-as-root
       "dscl . "
       (ecase ensure
         ((:absent)  "-delete ")
         ((:present) "-create "))
       (str "/Users/" sh-id)))
    (when uid
      (run-as-root
       "dscl . -create /Users/" sh-id
       " UniqueID "
       (sh-quote uid)))
    (when gid
      (run-as-root
       "dscl . -create /Users/" sh-id
       " PrimaryGroupID "
       (sh-quote gid)))
    (when home
      (run-as-root
       "dscl . -create /Users/" sh-id
       " NFSHomeDirectory "
       (sh-quote home)))
    (when shell
      (run-as-root
       "dscl . -create /Users/" sh-id
       " UserShell "
       (sh-quote shell)))
    (dolist (group (get-probed user :groups))
      (unless (find (resource-id group) groups)
        (run-as-root
         "dscl . -delete /Groups/" (sh-quote (resource-id group))
         " GroupMembership " sh-id)))
    (dolist (group groups)
      (run-as-root
       "dscl . -append /Groups/" (sh-quote group)
       " GroupMembership " sh-id))))

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
