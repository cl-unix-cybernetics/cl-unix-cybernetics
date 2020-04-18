;;
;;  adams - system administrator written in Common Lisp
;;
;;  Copyright 2020 Thomas de Grivel <thoxdg@gmail.com>
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
