(let ((qld (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file qld) (load qld)))

(mapc (lambda (s) (ignore-errors (ql:quickload s)))
      '(:sxql
        :serapeum
        :defclass-std
        :function-cache
        :access
        :pythonic-string-reader
        :dbi
        :dbd-sqlite3
        :cl-log
        :hunchentoot
        :clack
        :yason
        :cl-ppcre
        :quri
        :alexandria))

;; enable pythonic reader only if a known function exists
(let* ((pkg (find-package :pythonic-string-reader))
       (fn  (and pkg (or (find-symbol "ENABLE" pkg)
                         (find-symbol "ENABLE-READER" pkg)
                         (find-symbol "ENABLE-PYTHONIC-STRING-READER" pkg)))))
  (when (and fn (fboundp fn)) (funcall fn)))
