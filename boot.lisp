;; --- boot.lisp ---

;; Ensure the target package exists so (in-package :abstock) in your files won't choke.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :abstock)
    (defpackage :abstock (:use :cl))))

;; Load Quicklisp if present (home or project-local).
(let ((ql (or (probe-file (merge-pathnames "quicklisp/setup.lisp"
                                           (user-homedir-pathname)))
              (probe-file "quicklisp/setup.lisp"))))
  (when ql (load ql)))

;; Make sure ASDF is available.
(require 'asdf)

;; Load ALL external deps that abstock.lisp (and friends) read/require.
;; (These names match what showed up in your backtraces.)
(when (find-package :ql)
  (funcall (read-from-string "ql:quickload")
           '(:alexandria
             :serapeum
             :sxql
             :defclass-std
             :function-cache
             :access
             :pythonic-string-reader
             :dbi
             :dbd-sqlite3
             :cl-log          ;; provides the LOG package your code uses
             :hunchentoot
             :clack
             :yason
             :cl-ppcre
             :quri
             :cl-slug         ;; SLUG::
             :str             ;; STR::
             :parse-float     ;; PARSE-FLOAT::
             :cl-cron         ;; CL-CRON::
             :unix-opts)))    ;; OPTS::

;; 1) Load core sources first (everything in src/ except config.lisp and web.lisp).
(let* ((src (merge-pathnames "src/" (truename "./")))
       (all (directory (merge-pathnames "*.lisp" src)))
       (core (remove-if (lambda (p)
                          (member (string-downcase (file-namestring p))
                                  '("config.lisp" "web.lisp")))
                        all)))
  (dolist (f (sort core #'string< :key #'namestring))
    (load f)))

;; 2) Apply site configuration (relies on deps and globals loaded above).
(load "config.lisp")

;; 3) Start web entrypoint last.
(load "src/web.lisp")
