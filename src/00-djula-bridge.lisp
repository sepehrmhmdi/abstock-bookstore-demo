(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :djula)
    (defpackage :djula
      (:use :cl)
      (:export :*template-dirs* :add-template-directory
               :compile-template* :render-template*)))
  (in-package :djula))

(defparameter *template-dirs* '())

(defun add-template-directory (dir)
  (pushnew dir *template-dirs* :test #'equal))

(defun compile-template* (name) name)

(defun render-template* (tpl stream &rest context)
  (declare (ignore context))
  (format stream "<!-- djula shim rendered: ~A -->" tpl))
