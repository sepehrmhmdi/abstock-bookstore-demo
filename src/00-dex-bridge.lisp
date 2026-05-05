(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dex)
    (defpackage :dex (:use :cl) (:export :get :post :request :http-request))))

(in-package :dex)

;; Simple wrappers so code that expects the DEX package keeps working:
(defun get (&rest args) (apply #'dexador:get args))
(defun post (&rest args) (apply #'dexador:post args))
(defun request (&rest args) (apply #'dexador:request args))
;; There is no exported dexador:http-request; map it to dexador:request.
(defun http-request (&rest args) (apply #'dexador:request args))
