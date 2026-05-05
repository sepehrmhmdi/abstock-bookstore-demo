(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :uuid)
    (defpackage :uuid (:use :cl)
      (:export :make-v4-uuid :uuid-to-string)))
  (in-package :uuid))

;; Very small v4 generator (good enough as a stub).
(defun make-v4-uuid ()
  (let ((b (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref b i) (random 256)))
    ;; Set version (0100) and variant (10).
    (setf (aref b 6) (logior #x40 (logand (aref b 6) #x0F)))
    (setf (aref b 8) (logior #x80 (logand (aref b 8) #x3F)))
    b))

(defun uuid-to-string (u)
  (flet ((hx (i) (format nil "~2,'0x" (aref u i))))
    (concatenate 'string
      (hx 0)(hx 1)(hx 2)(hx 3) "-"
      (hx 4)(hx 5) "-"
      (hx 6)(hx 7) "-"
      (hx 8)(hx 9) "-"
      (hx 10)(hx 11)(hx 12)(hx 13)(hx 14)(hx 15))))
