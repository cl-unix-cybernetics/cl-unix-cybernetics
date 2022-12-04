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
