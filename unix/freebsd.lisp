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

(defun get-sh-var (name file)
  (let (value)
    (with-sh-var (var val) (egrep (str "^" name "=") file)
      (when (string= name var)
        (setq value val)))
    value))

(defsetf get-sh-var (name file) (value)
  `(let ((name (re-quote ,name))
         (value (re-quote ,value))
         (file (sh-quote ,file)))
     (run "perl -pi -e " (sh-quote (str "s/^" name "=.*$/" name "=" value "/"))
          file)))
    
(defmethod probe-hostname ((host host) (os os-freebsd))
  (let ((hostname (run-1 "hostname"))
        (rc-conf (get-sh-var "hostname" "/etc/rc.conf")))
    (list :hostname (if (equal hostname rc-conf)
                        hostname
                        (list hostname :rc-conf rc-conf)))))

(defmethod op-hostname ((host host) (os os-freebsd) &key hostname)
  (let* ((probed (get-probed host :hostname))
         (probed-rc probed))
    (when (consp probed)
      (setq probed (first probed)
            probed-rc (getf (rest probed-rc) :rc-conf probed)))
    (unless (equal hostname probed)
      (call-next-method))
    (unless (equal hostname probed-rc)
      (setf (get-sh-var "hostname" "/etc/rc.conf") hostname)))) 

(defmethod op-update-group ((group group) (os os-freebsd) &key ensure gid)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "pw groupdel")
               ((:present) "pw groupadd")
               ((nil)      "pw groupmod"))
             (when gid `("-g" ,(sh-quote gid)))
             "-n" (sh-quote (resource-id group)))))

(defmethod op-update-user ((user user) (os os-freebsd) &key ensure uid gid
                                                         realname home shell
                                                         login-class
                                                         groups)
  (run-as-root
   (join-str " "
             (ecase ensure
               ((:absent)  "pw userdel")
               ((:present) "pw useradd")
               ((nil)      "pw usermod"))
             (when realname `("-c" ,(sh-quote realname)))
             (when home `("-d" ,(sh-quote home)))
             (when gid `("-g" ,(sh-quote gid)))
             (when login-class `("-L" ,(sh-quote login-class)))
             (when groups `("-G" ,(join-str "," (mapcar #'sh-quote groups))))
             (when shell `("-s" ,(sh-quote shell)))
             (when uid `("-u" ,(sh-quote uid)))
             "-n" (sh-quote (resource-id user)))))

;;  Pkg

(defun freebsd-pkg-version<8>-status (str)
  (cond ((equal "=" str) :latest)
        ((equal "<" str) :update-available)
        ((equal ">" str) :newer)
        ((equal "?" str) :not-in-index)
        ((equal "!" str) :error)
        (:otherwise nil)))

(define-syntax freebsd-pkg-version<8> (name version
                                       (#'freebsd-pkg-version<8>-status status))
    #~|^([-_0-9A-Za-z]+)-([_.,0-9A-Za-z]+)\s+([=<>?!])$|)

(defmethod probe-host-packages ((host host) (os os-freebsd))
  (let ((packages))
    (with-freebsd-pkg-version<8> (name version ensure)
        (run "pkg version")
      (when (and name version ensure)
        (let ((pkg (resource 'freebsd-pkg name))
              (versions (list version)))
          (add-probed-properties pkg (properties* versions ensure))
          (push pkg packages))))
    (properties :packages (nreverse packages))))

(define-resource-class freebsd-pkg (pkg)
  ()
  ((probe-freebsd-pkg :properties (:ensure :versions)))
  ((op-freebsd-pkg :properties (:ensure :versions))))

(defmethod probe-freebsd-pkg ((pkg freebsd-pkg) (os os-freebsd))
  (let ((id (resource-id pkg))
        (ensure :absent)
        versions)
    (with-freebsd-pkg-version<8> (name ver status)
        (run "pkg version | egrep ^" (sh-quote id) "-")
      (when (equal id name)
        (setq ensure status)
        (push ver versions)))
    (properties* ensure versions)))

(defmethod op-freebsd-pkg ((pkg freebsd-pkg) (os os-freebsd)
                           &key ensure versions)
  (let ((id (resource-id pkg))
        (cmd (ecase ensure
               ((:absent) "pkg delete -y ")
               ((:present :latest) "pkg install -y "))))
    (if versions
        (dolist (ver versions)
          (run-as-root cmd (sh-quote id "-" ver)))
        (run-as-root cmd (sh-quote id)))))
