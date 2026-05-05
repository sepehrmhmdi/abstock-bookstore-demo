;;;; abstock.asd

(in-package :asdf-user)

(asdf:defsystem "abstock"
  :version "0.11"
  :author "vindarel"
  :license "GPL3"
  :description "Abelujo's DB as a simple website for clients."

  :depends-on (
    ;; concurrency
    :bordeaux-threads

    ;; JSON
    :cl-json :yason :jonathan

    ;; web client & utils
    :dexador :mito :sxql :str :cl-slug :local-time :cl-cron :function-cache
    :cl-ppcre :parse-float :defclass-std :pythonic-string-reader
    :group-by :arrows :serapeum :uuid :alexandria :quri :clack :access

    ;; db
    :dbi :dbd-sqlite3

    ;; web app
    :hunchentoot :easy-routes :djula

    ;; scraping
    :lquery

    ;; scripting
    :unix-opts :cl-ansi-text

    ;; logging
    :log4cl :cl-log
  )

  :weakly-depends-on (:swank :sentry-client.async :sentry-client.hunchentoot)

  :components
  ((:module "src/loaders" :serial t
    :components
    ((:file "package")
     (:file "conditions")
     (:file "txt-loader")))
   (:module "src/datasources" :serial t
    :components
    ((:file "packages")
     (:file "librairiedeparis")))
   (:module "src" :serial t
    :components
    ( ;; early shims/bootstrap (keep first)
     (:file "00-abstock-early")   ; defines package :abstock and *selection*
     (:file "00-dex-bridge")      ; DEX->Dexador bridge

     ;; core
     (:file "abstock")
     (:file "pagination")
     (:file "currencies")
     (:file "00-currencies-shim") ; stub FORMAT-PRICE if not provided
     (:file "utils")
     (:file "user-content")
     (:file "parameters")

     ;; package + public Shipping API (defines package, not providers)
     (:file "shipping")  ; => src/shipping.lisp

     ;; providers (files in src/shipping/)
     (:module "shipping-providers" :pathname "shipping" :serial t
      :components
      ((:file "colissimo"))) ; => src/shipping/colissimo.lisp

     ;; web layer & misc
     (:file "web")
     (:file "email")
     (:file "selection")
     (:file "api")
     (:file "system-utils"))))

  :build-operation "program-op"
  :build-pathname "abstock"
  :entry-point "abstock::main"

  :in-order-to ((test-op (test-op "abstock-test"))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
