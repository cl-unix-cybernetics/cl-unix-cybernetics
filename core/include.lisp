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

(defun include/resolve-filename (spec)
  (flet ((try (&rest parts)
           (let ((path (str parts)))
             (when (probe-file path)
               (return-from include/resolve-filename path)))))
    (try spec)
    (try spec ".adams")))

(defun include/resolve-filename! (spec)
  (or (include/resolve-filename spec)
      (error "(include ~S) => file not found.~%
Current directory : ~S" source *default-pathname-defaults*)))

(defun include (&rest sources)
  (let* ((head (cons 'list nil))
         (tail head)
         (eof (gensym "EOF")))
    (dolist (source sources)
      (let ((path (include/resolve-filename! source)))
        (with-open-file (in path
                            :element-type 'character
                            :external-format :utf-8)
          (loop
             (let ((form (read in nil eof)))
               (when (eq form eof)
                 (return))
               (setf (rest tail) (cons form nil)
                     tail (rest tail)))))))
    head))
