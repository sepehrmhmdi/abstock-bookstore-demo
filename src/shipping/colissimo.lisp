;;;; src/shipping/colissimo.lisp
;;;; Implémentation "réelle" :abstock/shipping
;;;; - Lit COLISSIMO_* dans l'env
;;;; - Appelle un endpoint REST (POST) qui renvoie des tarifs (ou fallback grille locale)
;;;; - Retourne des offres normalisées que le site sait afficher

(in-package :abstock/shipping)

;;;; -----------------------
;;;; Config / Environment
;;;; -----------------------

(defparameter *store-postcode* (or (uiop:getenv "AB_STORE_POSTCODE") "75000"))
(defparameter *store-city*     (or (uiop:getenv "AB_STORE_CITY")     "Paris"))
(defparameter *store-country*  (or (uiop:getenv "AB_STORE_COUNTRY")  "FR"))

;; Endpoint renseigné. Ex: un proxy Colissimo, Boxtal, Sendcloud,
;; ou microservice qui interroge le WS Colissimo.
(defparameter *colissimo-rates-url* (uiop:getenv "COLISSIMO_RATES_URL"))
(defparameter *colissimo-user*      (uiop:getenv "COLISSIMO_USER"))
(defparameter *colissimo-pass*      (uiop:getenv "COLISSIMO_PASS"))
(defparameter *colissimo-api-key*   (uiop:getenv "COLISSIMO_API_KEY"))

(defun %euros->cents (v)
  (labels ((as-float (x)
             (cond
               ((numberp x) (coerce x 'double-float))
               ((stringp x)
                (let* ((s (str:trim x))
                       (s (str:replace-all "," "." s))
                       (s (cl-ppcre:regex-replace-all "[^0-9.\\-]" s "")))
                  (handler-case (coerce (read-from-string s) 'double-float)
                    (error () 0.0))))
               (t 0.0))))
    (round (* 100 (as-float v)))))

;;;; -----------------------
;;;; Cart helpers minimal
;;;; -----------------------

(defun card-qty (c) (or (getf c :|qty|) (getf c :qty) 1))
(defun card-weight-g (c)
  "Poids en grammes. Si absent, on prend 300g par livre."
  (or (getf c :|weight_g|) (getf c :weight_g) 300))

(defun total-weight-g (cards)
  (reduce #'+ cards
          :key (lambda (c) (* (card-qty c) (card-weight-g c)))
          :initial-value 0))

;;;; -----------------------
;;;; HTTP JSON (Dexador + CL-JSON)
;;;; -----------------------

(defun %request-json (url &key (method :post) (payload nil))
  (let* ((headers (remove nil
                          (list
                           '("Content-Type" . "application/json")
                           (when *colissimo-api-key*
                             (cons "X-API-KEY" *colissimo-api-key*)))))
         (auth    (and *colissimo-user* *colissimo-pass*
                       (list *colissimo-user* *colissimo-pass*)))
         (body    (and payload (cl-json:encode-json-to-string payload)))
         (resp    (dexador:request url
                                   :method method
                                   :headers headers
                                   :content body
                                   :basic-auth auth
                                   :connect-timeout 10
                                   :read-timeout 10)))
    (when (and resp (plusp (length resp)))
      (cl-json:decode-json-from-string resp))))

;;;; -----------------------
;;;; Normalisation d'offres
;;;; -----------------------

(defun normalize-offers (arr)
  "arr = liste d'alists comme:
    ((\"id\" . \"home\") (\"label\" . \"Colissimo Domicile\") (\"price\" . 690) (\"eta\" . \"J+2\"))
   Retourne des plists (:id :provider :label :eta :price-centimes)"
  (mapcar (lambda (o)
            (let ((id (or (cdr (assoc "id" o :test #'string=))
                          (getf o :id)))
                  (provider (or (cdr (assoc "provider" o :test #'string=))
                                (getf o :provider) "Colissimo"))
                  (label (or (cdr (assoc "label" o :test #'string=))
                             (getf o :label)))
                  (eta (or (cdr (assoc "eta" o :test #'string=))
                           (getf o :eta)))
                  (price (or (cdr (assoc "price" o :test #'string=))
                             (getf o :price))))
              ;; price peut être euros (nombre) ou centimes (entier) -> on force en centimes
              (let ((cents (if (and (integerp price) (>= price 50))
                               price
                               (%euros->cents price))))
                (list :id (princ-to-string id)
                      :provider provider
                      :label label
                      :eta eta
                      :price cents))))
          arr))

;;;; -----------------------
;;;; Fallback grille locale
;;;; -----------------------

(defun local-grid-rates (addr cards)
  "Tarifs de secours si pas d'API dispo. Adapte au besoin.
   FR métropole, poids <= 2kg."
  (declare (ignore addr))
  (let* ((w (total-weight-g cards))
         (band (cond
                 ((<= w 250)  490)   ; 4,90€
                 ((<= w 500)  590)
                 ((<= w 1000) 790)
                 ((<= w 2000) 890)
                 (t           1290))))
    (normalize-offers
     (list
      (list (cons "id" "home")
            (cons "provider" "Colissimo")
            (cons "label" "Colissimo Domicile")
            (cons "eta" "J+2")
            (cons "price" band))
      (list (cons "id" "pickup")
            (cons "provider" "Colissimo")
            (cons "label" "Point Retrait")
            (cons "eta" "J+2")
            (cons "price" (max 450 (- band 200))))))))

;;;; -----------------------
;;;; Appels tarifs (API)
;;;; -----------------------

(defun fetch-colissimo-rates (addr cards)
  "Attendu (côté endpoint) : JSON:
    { from:{cp,city,country}, to:{cp,city,country}, weight_g:1234 }
   Réponse: tableau d’offres [{id,label,eta,price}, ...]"
  (when *colissimo-rates-url*
    (let* ((payload (list
                     :from (list :cp *store-postcode*
                                 :city *store-city*
                                 :country *store-country*)
                     :to   (list :cp   (or (getf addr :cp)   (getf addr :|cp|)   "")
                                 :city (or (getf addr :ville) (getf addr :|ville|) "")
                                 :country (or (getf addr :pays) (getf addr :|pays|) "FR"))
                     :weight_g (total-weight-g cards)))
           (json (%request-json *colissimo-rates-url* :method :post :payload payload))
           ;; Le service doit répondre une liste. Si c’est un objet {offers:[...]}, on récupère la clé.
           (arr (or (and (listp json) json)
                    (cdr (assoc "offers" json :test #'string=))
                    (getf json :offers))))
      (when arr (normalize-offers arr)))))

;;;; -----------------------
;;;; API publique du module
;;;; -----------------------

(defun rates-for-address (addr cards)
  "Toujours retourner une liste d’offres (:id :provider :label :eta :price) en CENTIMES."
  (or (ignore-errors (fetch-colissimo-rates addr cards))
      (local-grid-rates addr cards)))

(defun find-rate-by-id (offers id)
  (find (princ-to-string id) offers
        :key  (lambda (o) (getf o :id))
        :test #'string=))
