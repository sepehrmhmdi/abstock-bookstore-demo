;;;; run.lisp — start abstock via ASDF only (no boot.lisp)

(in-package #:cl-user)

;; Always run from this file's directory (so relative paths work).
(uiop:chdir (uiop:pathname-directory-pathname *load-truename*))

;; 1) Load Quicklisp if available (nice to have, not required).
(let ((qld (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file qld)
    (format t "~&[run] Loading Quicklisp from ~A~%" qld)
    (load qld)))

;; 2) Register the local ASDF system BEFORE loading any project files.
(let ((asd (merge-pathnames "abstock.asd" (truename "./"))))
  (unless (probe-file asd)
    (error "[run] abstock.asd not found at ~A" asd))
  (asdf:load-asd asd)
  (format t "~&[run] Registered system from ~A~%" asd))

;; 3) Load the system (Quicklisp if present, otherwise plain ASDF).
(handler-case
    (progn
      #+quicklisp
      (progn (format t "[run] ql:quickload :abstock~%") (ql:quickload :abstock))
      #-quicklisp
      (progn (format t "[run] asdf:load-system :abstock~%") (asdf:load-system :abstock)))
  (error (c)
    (format *error-output* "~&[run] Failed to load system :abstock~%~A~%" c)
    (sb-ext:exit :code 1)))

;; 4) Run the entrypoint: prefer ABSTOCK::MAIN, fallback to ABSTOCK:START.
(let* ((pkg  (or (find-package :abstock)
                 (error "[run] Package ABSTOCK not present after load.")))
       (sym  (or (find-symbol "MAIN" pkg)
                 (find-symbol "START" pkg)))
       (fn   (and sym (fboundp sym) (symbol-function sym))))
  (unless fn
    (format *error-output* "~&[run] No ABSTOCK::MAIN or ABSTOCK:START function found.~%")
    (sb-ext:exit :code 1))
  (format t "~&[run] starting via ~A:~A~%" (package-name pkg) (symbol-name sym))
  (force-output)
  (let ((ret (funcall fn)))
    (format t "~&[run] entrypoint returned: ~S~%" ret)
    (force-output)))

;; 5) If the entrypoint returns (e.g., server in a thread), keep the process alive.
(loop (sleep 3600))
