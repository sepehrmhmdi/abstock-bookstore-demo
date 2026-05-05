;;
;; Configuration variables, loaded at startup.
;;

;; (in-package :abstock-user)

;; Enable the triple-quotes pythonic syntax.
;; Easier to write text with simple quotes.
(pythonic-string-reader:enable-pythonic-string-syntax)

(setf *port* 8989)

(setf *api-token* "change-me-dev-token")

(setf *contact-infos*
  '(:|phone| "01 00 00 00 00"
    :|phone2| ""
    :|email| "contact@example.com"))

;; SendGrid config:
(setf *email-config*
   '(:|sender-api-key| "change-me-email-api-key"
     :|from| "noreply@example.com"
     :|to| "bookstore@example.com"))

;; Simple anti-script-kiddy question for the validation form:
(setf *secret-question* "What is the demo word?")
(setf *secret-answer* "demo")

;; Theme
;; Themes are defined in src/templates/themes/<yourtheme>/
(setf *theme* nil)

;; Content.

(setf (user-content-brand-name *user-content*)
      "Demo Bookshop")

(setf (user-content-brand-home-title *user-content*)
      "Shop Online")

(setf (user-content-brand-link *user-content*)
      "https://example.com")

(setf (user-content-brand-link-title *user-content*)
      "https://example.com")

(setf (user-content-brand-contact-link *user-content*)
      "https://example.com/contact")

(setf (user-content-welcome-image *user-content*)
      nil)  ;; "path/to/img.png"

(setf (user-content-welcome-text *user-content*) "Welcome!")

(setf (user-content-welcome-second-text *user-content*)
      """"
      <p>
      You can contact us at:

      <ul>
      <li>+33 (0)1 00 00 00 00</li>
      </ul>
      </p>
      """")

;;;
;;; Sélection du libraire.
;;;
(setf (user-content-enable-product-selection *user-content*)
      nil)

(setf (user-content-product-selection-short-name *user-content*)
      ;; mainly for the navbar button. Defaults to "Sélection du libraire".
      """"Our selection"""")

(setf (user-content-product-selection-intro-text *user-content*)
      nil)

;;
;; Shopping basket.
;;
(setf (user-content-basket-title *user-content*)
      ;; Basket page title
      "Your basket")

(setf (user-content-basket-short-name *user-content*)
      ;; button
      "Basket")

(setf (user-content-basket-text *user-content*)
      """"
          <p>
          You are nearly done. Fill in the validation form below and we'll come back to you.<br/>
          Thank you!
          </p>
      """"
)

(setf (user-content-basket-show-validation-form *user-content*)
      ;; Show the validation form: already true by default.
      t)

;;; Additional headers.
(setf (user-content-additional-headers *user-content*)
      """"
      HTML.
      You can put analytics or custom headers here.
      """")

(setf *ignore-shelves-starting-by* '("test-" "TEST"))