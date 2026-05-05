(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :abstock)
    (defpackage :abstock (:use :cl)))
  ;; If JOJO doesn't exist, provide a minimal one that wraps YASON.
  (unless (find-package :jojo)
    (defpackage :jojo
      (:use :cl)
      (:export :encode :decode :stringify :parse :to-json :from-json))))

(in-package :jojo)

;; Minimal API most code expects; mapped to YASON.
(defun stringify (obj &key stream)
  (if stream
      (yason:encode obj stream)
      (with-output-to-string (s)
        (yason:encode obj s))))

(defun encode   (obj &key stream) (stringify obj :stream stream))
(defun to-json  (obj &key stream) (encode    obj :stream stream))

(defun parse  (json) (yason:parse json))
(defun decode (json) (parse json))
(defun from-json (json) (parse json))
