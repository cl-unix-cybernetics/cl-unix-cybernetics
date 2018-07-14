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

(in-re-readtable)

(define-resource-class ssh-authorized-key ()
  ()
  ((probe-ssh-authorized-key :properties (:ensure :name :pubkey :type)))
  ((op-ssh-authorized-key :properties (:ensure :name :pubkey :type))))

(define-syntax ssh-public-key (type pubkey name)
    #~|([^\s]+)\s+([^\s]+)\s+(.*)|)

(defun ssh-authorized-key (key)
  (multiple-value-bind (type pubkey name) (parse-ssh-public-key key)
    (resource 'ssh-authorized-key name
              :type type
              :pubkey pubkey
              :name name)))

(defmethod probe-ssh-authorized-key ((res ssh-authorized-key)
                                     (os os-unix))
  (let* ((spec-type (the string (get-specified res :type)))
         (spec-pubkey (the string (get-specified res :pubkey)))
         (user *parent-resource*)
         (home (resource-id (get-probed user :home)))
         (path (str home "/.ssh/authorized_keys"))
         (file (with-parent-resource *host*
                 (resource 'file path)))
         (ensure :absent))
    (multiple-value-bind (type pubkey name)
        (unless (eq :absent (get-probed file :ensure))
          (with-ssh-public-key (type pubkey name)
              (run "cat " (sh-quote path))
            (when (and (string= spec-type (the string type))
                       (string= spec-pubkey (the string pubkey)))
              (setq ensure :present)
              (return (values type pubkey name)))))
      (properties* ensure type pubkey name))))

(defmethod op-ssh-authorized-key ((res ssh-authorized-key)
                                  (os os-unix)
                                  &key ensure type pubkey name)
  (declare (type symbol ensure))
  (let* ((user *parent-resource*)
         (home (resource-id (get-probed user :home)))
         (dot-ssh (str home "/.ssh"))
         (ak (str dot-ssh "/authorized_keys"))
         (sh-ak (sh-quote ak))
         (sh-ak-tmp (sh-quote (str ak ".tmp"))))
    (setf type (get-specified res :type)
          pubkey (get-specified res :pubkey)
          name (get-specified res :name))
    (force-output)
    (when (position ensure '(:absent nil))
      (run "grep -v " (sh-quote pubkey) " " sh-ak " > " sh-ak-tmp)
      (run "mv " sh-ak-tmp " " sh-ak))
    (when (position ensure '(:present nil))
      (run "echo " (sh-quote type " " pubkey " " name) " >> " sh-ak))))

(defmethod resource-additional-specs ((res ssh-authorized-key)
                                      (os os-unix))
  (let* ((user *parent-resource*)
         (home (resource-id (get-specified user :home)))
         (ssh-dir (str home "/.ssh"))
         (ak (str dot-ssh "/authorized_keys")))
    (with-parent-resource *host*
      (resource 'directory ssh-dir
                :ensure :present
                :mode #o700)
      (resource 'file ak
                :ensure :present
                :mode #o600))))
