;;
;; Configuration variables, loaded at startup.
;;

;; (in-package :abstock)

(ignore-errors (pythonic-string-reader:enable-pythonic-string-syntax))

;; 2) Réseau / Admin
(setf *port* 8989)
(setf *api-token* "change-me-dev-token")

;; 3) Contact boutique
(setf *contact-infos*
  '(:|phone|  "01 00 00 00 00"
    :|phone2| ""
    :|email|  "contact@example.com"))

;; 4) Email (SendGrid ou autre) — placeholders OK
(setf *email-config*
   '(:|sender-api-key| "change-me-email-api-key"
     :|from|           "noreply@example.com"
     :|to|             "bookstore@example.com"))

;; 5) Anti-spam (form panier)
(setf *secret-question* "What is the demo word?")
(setf *secret-answer*   "demo")

;; 6) Thème
(setf *theme* "sepehrtheme")

;; 7) Contenu vitrine
(setf (user-content-brand-name *user-content*)         "Demo Bookshop")
(setf (user-content-brand-home-title *user-content*)   "Shop Online")
(setf (user-content-brand-link *user-content*)         "https://example.com")
(setf (user-content-brand-link-title *user-content*)   "https://example.com")
(setf (user-content-brand-contact-link *user-content*) "https://example.com/contact")

(setf (user-content-welcome-image *user-content*) nil)
(setf (user-content-welcome-text *user-content*) "Welcome!")
(setf (user-content-welcome-second-text *user-content*)
      "
<p>
You can contact us at:
<ul>
  <li>+33 (0)1 00 00 00 00</li>
</ul>
</p>
")

;;; Sélection du libraire
(setf (user-content-enable-product-selection *user-content*) nil)
(setf (user-content-product-selection-short-name *user-content*) "Our selection")
(setf (user-content-product-selection-intro-text *user-content*) nil)

;;; Panier (textes)
(setf (user-content-basket-title *user-content*) "Your basket"
      (user-content-basket-short-name *user-content*) "Basket"
      (user-content-basket-text *user-content*)
      "
<p>
You are nearly done. Fill in the validation form below and we'll come back to you.<br/>
Thank you!
</p>
")
(setf (user-content-basket-show-validation-form *user-content*) t)

;;; En-têtes additionnels (Matomo, etc.)
(setf (user-content-additional-headers *user-content*)
      "
<!-- Optional analytics/custom headers go here. -->
")

(setf *ignore-shelves-starting-by* '("test-" "TEST"))

;; ------------------------------------------------------------
;; 8) SHIPPING / COLISSIMO — demo store location
;; ------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (ignore-errors
    (let* ((pkg (find-package :abstock/shipping))
           (sym-post (and pkg (find-symbol "*STORE-POSTCODE*" pkg)))
           (sym-city (and pkg (find-symbol "*STORE-CITY*"     pkg)))
           (sym-ctry (and pkg (find-symbol "*STORE-COUNTRY*"  pkg))))
      (when (and sym-post sym-city sym-ctry)
        (setf (symbol-value sym-post) "00000"
              (symbol-value sym-city) "Demo City"
              (symbol-value sym-ctry) "FR")))

    ;; Si un endpoint Colissimo existe, décommente et renseigne avec des variables d'environnement.
    ;; Ne commit jamais de vraie clé API ici.
    ;; (let* ((pkg (find-package :abstock/shipping))
    ;;        (rurl (and pkg (find-symbol "*COLISSIMO-RATES-URL*" pkg)))
    ;;        (usr  (and pkg (find-symbol "*COLISSIMO-USER*"      pkg)))
    ;;        (pwd  (and pkg (find-symbol "*COLISSIMO-PASS*"      pkg)))
    ;;        (akey (and pkg (find-symbol "*COLISSIMO-API-KEY*"   pkg))))
    ;;   (when rurl (setf (symbol-value rurl) "https://example.com/rates"))
    ;;   ;; (when usr  (setf (symbol-value usr)  "example-login"))
    ;;   ;; (when pwd  (setf (symbol-value pwd)  "example-password"))
    ;;   ;; (when akey (setf (symbol-value akey) "example-api-key"))
    ;; )
    ))

;; ------------------------------------------------------------
;; 9) PAYMENT / STANCER — local proxy URL
;; ------------------------------------------------------------
(eval-when (:load-toplevel :execute)
  (let ((url "http://127.0.0.1:3031"))
    (if (boundp '*stancer-proxy-url*)
        (setf *stancer-proxy-url* url)
        (defparameter *stancer-proxy-url* url))))

;; NB:
;; - Si *COLISSIMO-RATES-URL* est défini, le module :abstock/shipping appellera le endpoint.
;; - Sinon, il utilisera une grille locale fallback.