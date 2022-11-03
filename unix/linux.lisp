;; Adams - UNIX system administration tool written in Common Lisp
;; Copyright 2013-2022 Thomas de Grivel <thodg@kmx.io>

(in-package :adams)

(in-re-readtable)

(define-syntax ls<1>-l--full-time (mode
                                   (#'sh-parse-integer links)
                                   owner
                                   group
                                   (#'sh-parse-integer size)
                                   (#'chronicity:parse time)
                                   name)
  #~|^([-a-zA-Z]{10})\s+([0-9]+)\s+(\S+)\s+(\S+)\s+([0-9]+)\s+(\S+ \S+ \S+)\s+(.+)$|
  "Syntax for `ls -l --full-time` output. See ls(1).")

(defmethod probe-vnode-using-ls ((vnode vnode) (os os-linux))
  (let ((id (resource-id vnode))
        (ensure :absent))
    (multiple-value-bind (mode links owner group size mtime)
        (with-ls<1>-l--full-time (mode links owner group size mtime name)
            (ls "-ld --full-time" (sh-quote id))
          (when (and name (string= id (the string name)))
            (setq mode (mode (mode-permissions mode))
                  owner (resource 'user owner)
                  group (resource 'group group)
                  ensure :present)
            (return (values mode links owner group size mtime))))
      (properties* ensure mode links owner group size mtime))))

(defun parse-hex (string)
  (parse-integer string :radix 16))

(define-syntax stat<1>-t (file
                          (#'sh-parse-integer size blksize)
                          (#'parse-hex mode)
                          (#'sh-parse-integer uid gid)
                          (#'parse-hex dev)
                          (#'sh-parse-integer ino links)
                          (#'parse-hex major minor)
			  (#'parse-unix-timestamp atime mtime ctime
                                                  birthtime)
			  (#'sh-parse-integer optimal-io))
    #~|^(.*) ([0-9]+) ([0-9]+) ([A-Fa-f0-9]+) ([0-9]+) ([0-9]+) ([A-Fa-f0-9]+) ([0-9]+) ([0-9]+) ([A-Fa-f0-9]+) ([A-Fa-f0-9]+) ([0-9.]+) ([0-9.]+) ([0-9.]+) ([0-9.]+) ([0-9]+)$|
  "Syntax for terse stat(1) output on Linux.")

(defmethod probe-vnode-using-stat ((vnode vnode) (os os-linux))
  (let ((id (resource-id vnode))
        (ensure :absent))
    (multiple-value-bind #1=(size blksize mode uid gid dev ino links
                                  major minor atime mtime ctime
                                  birthtime optimal-io)
        (with-stat<1>-t (name . #1#) (stat "-t" (sh-quote id))
          (when (and name (string= id (the string name)))
            (setq mode (mode (mode-permissions mode))
                  ensure :present)
            (return (values* #1#))))
        (properties* (ensure . #1#)))))
