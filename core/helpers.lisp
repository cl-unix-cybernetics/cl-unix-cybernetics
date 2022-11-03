;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(defun read-file (&rest path-parts)
  (let ((path (str path-parts)))
    (with-output-to-string (out)
      (with-open-file (stream path :element-type 'character)
        (let ((buf (make-string 4096)))
          (loop
             (let ((size (read-sequence buf stream)))
               (when (zerop size)
                 (return))
               (write-sequence buf out :end size))))))))

(defun static-file (path &rest plist)
  (resource 'file path
            :content (read-file (hostname *host*) path)
            plist))

(defun static-directory (directory &rest plist)
  (let* ((host-dir (truename (pathname (the string
                                            (str (hostname) #\/)))))
         (dir-path (str host-dir directory)))
    (print dir-path)
    (mapcar (lambda (path)
              (let ((name (enough-namestring path host-dir)))
              (resource 'file (str "/" name)
                        :content (read-file path)
                        plist)))
                  (directory dir-path))))
