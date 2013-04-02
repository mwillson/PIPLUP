;; Setup project to use cl-sqlite library

(load "/home/shanghaiden/Desktop/projects/lisp/systems/quicklisp.lisp")
(load "/home/shanghaiden/quicklisp/setup.lisp")
(ql:quickload "clsql")

(in-package #:clsql-user)

(clsql:def-view-class pword ()
  ((wordid
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :wordid)
   (word-root
    :accessor word-root
    :type (string 30)
    :initarg :word-root)
   (endsound
    :accessor endsound
    :type symbol
    :initarg :endsound)
   (syllables
    :accessor syllables
    :type integer
    :initarg :syllables)
   (affixes
    :accessor affixes
    :type list
    :nulls-ok t
    :initarg :affixes)
  )
  (:base-table pword)
)

(load "~/quicklisp/dists/quicklisp/software/clsql-20130312-git/uffi/clsql-uffi.lisp")

