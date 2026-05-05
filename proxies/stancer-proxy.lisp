;; Minimal Stancer proxy (no sudo). Listens on 127.0.0.1:3031.
;; Env: STANCER_PUBLIC_KEY, STANCER_SECRET_KEY, PORT (opt), STANCER_BASE (opt).
;; Mount point: /stancer/...  (set AB_STANCER_PROXY_URL="http://127.0.0.1:3031/stancer")

(let ((qld (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file qld) (load qld)))

(ql:quickload '(:hunchentoot :dexador :cl-base64 :yason) :silent t)

(defpackage :stancer-proxy
  (:use :cl)
  (:import-from :hunchentoot
    :acceptor :easy-acceptor :start :stop :define-easy-handler
    :request-uri* :request-method* :headers-in* :raw-post-data
    :content-type* :return-code* :header-out)
  (:import-from :yason :with-output-to-string :encode)
  (:import-from :cl-base64 :usb8-array-to-base64-string))
(in-package :stancer-proxy)

(defun getenv (name &optional default) (or (uiop:getenv name) default))

(defparameter *port* (parse-integer (getenv "PORT" "3031")))
(defparameter *stancer-base*
  (let ((b (getenv "STANCER_BASE" "https://api.stancer.com/")))
    (if (and (> (length b) 0) (char= (char b (1- (length b))) #\/)) b (concatenate 'string b "/"))))

(defparameter *pub* (getenv "STANCER_PUBLIC_KEY"))
(defparameter *sec* (getenv "STANCER_SECRET_KEY"))

(when (or (null *pub*) (null *sec*))
  (format *error-output* "~&[stancer-proxy] Set STANCER_PUBLIC_KEY and STANCER_SECRET_KEY.~%")
  (uiop:quit 1))

(defun ascii-octets (s)
  (let* ((n (length s)) (v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v) (setf (aref v i) (char-code (char s i))))))

(defun basic-auth-header ()
  (let* ((raw (format nil "~a:~a" *pub* *sec*))
         (b64 (usb8-array-to-base64-string (ascii-octets raw))))
    (format nil "Basic ~a" b64)))

(defun join-url (base path)
  (cond
    ((and (> (length base) 0) (char= (char base (1- (length base))) #\/))
     (if (and (> (length path) 0) (char= (char path 0) #\/))
         (concatenate 'string base (subseq path 1))
         (concatenate 'string base path)))
    (t
     (if (and (> (length path) 0) (char= (char path 0) #\/))
         (concatenate 'string base path)
         (concatenate 'string base "/" path)))))

(defun copy-in-headers ()
  "Copy client headers, drop Host; add Authorization."
  (let ((hs (remove-if (lambda (cell) (string-equal (car cell) "host"))
                       (headers-in*))))
    (acons "Authorization" (basic-auth-header) hs)))

(defun upstream-url ()
  ;; Proxy under /stancer/... so strip that prefix when joining.
  (let* ((uri (request-uri*))
         (p (if (and (>= (length uri) 9) (string= (subseq uri 0 9) "/stancer/"))
                (subseq uri 9)
                uri)))
    (join-url *stancer-base* p)))

(defun http (method url headers &optional body)
  (let* ((content (dexador:request url
                    :method method
                    :headers headers
                    :content body
                    :connect-timeout 30
                    :read-timeout 60
                    :force-string t))
         (resp dexador:*last-response*)
         (code (ignore-errors (slot-value resp 'dexador::status-code)))
         (rh   (ignore-errors (slot-value resp 'dexador::headers))))
    (values content code rh)))

(define-easy-handler (health :uri "/_health") ()
  (setf (content-type*) "application/json")
  (yason:with-output-to-string (*standard-output*)
    (yason:encode `(("ok" . t) ("proxy" . "stancer") ("base" . ,*stancer-base*)))))

;; Only proxy the /stancer/* prefix
(define-easy-handler (proxy-stancer :uri "/stancer/*") ()
  (let* ((url (upstream-url))
         (meth (request-method*))
         (body (when (member meth '(:post :put :patch :delete) :test #'eq)
                 (raw-post-data t)))
         (headers (copy-in-headers)))
    (multiple-value-bind (content code rh) (http meth url headers body)
      ;; Propagate a couple of headers if available
      (when rh
        (dolist (h rh)
          (let ((k (string (car h))) (v (cdr h)))
            (cond
              ((string-equal k "content-type")
               (setf (content-type*) v))
              ((string-equal k "location")
               (setf (header-out "Location") v))))))
      (when code (setf (return-code*) code))
      (when (null (content-type*))
        (setf (content-type*) "application/json; charset=utf-8"))
      content)))

(let ((acc (make-instance 'easy-acceptor :address "127.0.0.1" :port *port*)))
  (start acc)
  (format t "~&[stancer-proxy] listening on http://127.0.0.1:~d -> ~a~%" *port* *stancer-base*)
  (finish-output)
  (loop (sleep 3600)))
