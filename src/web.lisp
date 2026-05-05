(in-package :abstock)
(eval-when (:compile-toplevel :load-toplevel :execute) (shadow 'get))

(in-package :abstock)
(eval-when (:compile-toplevel :load-toplevel :execute) (shadow 'get))

;;; Helper local : construit l'adresse à afficher.
;;; Retourne les values: l1 l2 ville cp pays txt is-relay relay-label
(defun %relay-choice->addr+txt (choice l1 l2 ville cp pays)
  (labels
      ((pick (&rest ks)
         (loop for k in ks
               for v = (and choice (getf choice k))
               when (and v (not (str:blankp (princ-to-string v))))
                 do (return v))))
    (let* ((r1   (pick :relay_addr1 :relay_address1 :relay_address :point_addr1 :point_address1 :address1))
           (r2   (pick :relay_addr2 :relay_address2 :point_addr2 :point_address2 :address2))
           (rcit (pick :relay_ville :relay_city :point_city :city))
           (rcp  (pick :relay_cp :relay_zip :relay_postcode :point_cp :zip :cp))
           (rcty (pick :relay_country :point_country :country :countryCode))
           (is-relais (or r1 rcit rcp))
           (L1 (or (and is-relais r1)   l1))
           (L2 (or (and is-relais r2)   l2))
           (V  (or (and is-relais rcit) ville))
           (CP (or (and is-relais rcp)  cp))
           (PY (or (and is-relais (%country->iso2 rcty)) pays))
           (txt (%addr->txt L1 L2 V CP PY))
           (label (or (and choice (getf choice :relay_name))
                      (and choice (getf choice :relay_label))
                      (and choice (getf choice :label)))))
      (values L1 L2 V CP PY txt is-relais label))))




(defun %to-cents (p)
  "Convertit prix *en euros* -> centimes. Si déjà en centimes (ex: 4175), le garde."
  (labels ((euros->cents* (x)
             (cond
               ((integerp x) x)
               ((numberp x) (round (* 100 x)))
               ((stringp x)
                (let* ((s (str:trim x))
                       (s (str:replace-all "," "." s))
                       (s (cl-ppcre:regex-replace-all "[^0-9.\\-]" s "")))
                  (round (* 100 (or (ignore-errors (read-from-string s)) 0)))))
               (t 0))))
    (let* ((raw (euros->cents* p)))
      (cond
        ((and (integerp p) (< p 1000)) (* 100 p))
        ((and (numberp p) (< raw 100)) (* 100 raw))
        (t raw)))))

(defun shipping-offers-for (ids l1 l2 ville cp pays)
  (let* ((cards (ignore-errors (cards-from-ids ids))))
    (handler-case
        (let ((pays* (%country->iso2 pays)))
          (abstock/shipping:rates-for-address
           (list :l1 l1 :l2 l2 :ville ville :cp cp :pays pays*)
           (or cards '())))
      (error () '()))))



(defclass abstock-acceptor (easy-routes:easy-routes-acceptor) ())

(defmethod hunchentoot:acceptor-dispatch-request :around ((acc abstock-acceptor) req)
  (let ((p (hunchentoot:script-name req)))
    (when (or
           (member p '("/panier" "/panier/count"
                       "/checkout/adresse" "/checkout/livraison" "/checkout/payment"
                       "/paiement/carte" "/paiement/carte/retour" "/paiement/carte/confirm"
                       "/mon-compte" "/mon-compte/editer" "/mon-compte/mot-de-passe"
                       "/connexion" "/inscription")
                   :test #'string=)
           (str:starts-with? "/commande/" p)
           (str:starts-with? "/mon-compte/adresses/" p)
           (str:starts-with? "/api/cart" p)
           (str:starts-with? "/api/shipping" p))
      (setf (hunchentoot:header-out :cache-control) "no-store")))
  (call-next-method))




;;; --- Templates globals (pré-déclaration pour SBCL) ------------------------
(defvar +BASE.HTML+ nil) (defvar +WELCOME.HTML+ nil) (defvar +CARDS.HTML+ nil)
(defvar +SELECTION-PAGE.HTML+ nil) (defvar +CARD-PAGE.HTML+ nil)
(defvar +ADMIN-PAGE.HTML+ nil) (defvar +CONTACT.HTML+ nil)
(defvar +CONNEXION.HTML+ nil) (defvar +INSCRIPTION.HTML+ nil)
(defvar +AGENDA.HTML+ nil) (defvar +PANIER.HTML+ nil)
(defvar +COMMAND-CONFIRMED.HTML+ nil) (defvar +ERROR-MESSAGES.HTML+ nil)
(defvar +404.HTML+ nil) (defvar +MON-COMPTE.HTML+ nil)
(defvar +ADDRESS-FORM.HTML+ nil) (defvar +ORDER-DETAILS.HTML+ nil)
(defvar +EDIT-PROFILE.HTML+ nil) (defvar +CHANGE-PASSWORD.HTML+ nil)
(defvar +PAY-CARD-STUB.HTML+ nil)


;;; ===========================================================================
;;; web.lisp — Drop-in complet, robuste, prêt à coller
;;; - Ajouts : valeurs par défaut, fallback de fonctions, robustesse SQLite,
;;;            parsing sûr, vérifs d'inputs, messages d'erreurs propres.
;;; ===========================================================================

;; ------------------------------
;; Defaults & fallbacks (no-op)
;; ------------------------------
(unless (boundp '*version*)          (defparameter *version* "dev"))
(unless (boundp '*db-app-name*)      (defparameter *db-app-name* "abstock"))
(unless (boundp '*db-name*)          (defparameter *db-name* "abstock"))
(unless (fboundp 'format-box)
  (defun format-box (stream msg) (format stream "~a~%" msg)))
(unless (fboundp 'email-send)
  (defun email-send (&key to from reply-to subject content)
    (declare (ignorable to from reply-to subject content))
    (log:warn "email-send fallback: no-op (configure *email-config* / sender).")
    :ok))
(unless (fboundp 'user-content-brand-name)
  (defun user-content-brand-name (uc)
    (declare (ignorable uc)) "ABStock"))
(unless (boundp '*secret-question*)
  (defparameter *secret-question* "Écrivez « livre »"))
;; Pour (#.*card-page-url*)
(unless (boundp '*card-page-url*)
  (defparameter *card-page-url* "/livre/:slug"))
(unless (boundp '*card-page-url-name*)
  (defparameter *card-page-url-name* "livre"))

;; Utilitaire: safe string predicate
(defun non-empty-string-p (x) (and (stringp x) (plusp (length (str:trim x)))))

(defun blankish-p (s)
  "Vrai si s est vide ou un faux vide rendu par les templates."
  (or (str:blankp s)
      (string= s "''") (string= s "\"\"")
      (string-equal s "nil") (string-equal s "null")))

(defun pick-param (param fallback)
  "Renvoie PARAM si non vide, sinon FALLBACK."
  (if (blankish-p param) fallback param))


;;
;; ────────────────────────────────────────────────────────────────────────────
;; App variables
;; ────────────────────────────────────────────────────────────────────────────
;;

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defvar *api-token* nil
  "Basic security: API secret token to set in the config.lisp file. If it isn't set, the admin user won't be able to edit texts from the \"admin\" page.")

(defvar *user-template-lock* (bt:make-lock)
  "Lock to save user HTML edited in the admin.")

(defvar *admin-uuid* nil
  "UUID used to build the admin URL.")

(defvar *admin-url* "/uuid-admin"
  "Admin UUID url, needs to be built with (get-admin-url).")

(defparameter *use-admin-custom-texts* t
  "If non true, don't read the saved admin content. Bypass it. We'll read the config only.
  Dev and debugging purposes")

(defparameter *start-swank-server* t
  "If non true, don't start a Swank server.")

(defvar *sentry-dsn-file* "~/.config/abstock/sentry-dsn.txt")

(defvar *email-config*
  '(:|sender-api-key| ""
    :|from| ""
    :|to| ""))

(defparameter *port* 8989
  "We can override it in the config file.")

(defparameter *dev-mode* nil
  "If non-nil, don't use Sentry and use a subset of all the cards.")

(defvar *selection* nil
  "List of cards for the selection page.")

(defparameter *theme* ""
  "Custom theme name (string).
  The theme templates are located at src/templates/<theme>/.")

;; ===== Stancer proxy config (env-aware) =====
(defparameter *stancer-proxy-url*
  (or (uiop:getenv "AB_STANCER_PROXY_URL")
      (uiop:getenv "STANCER_PROXY_URL")
      "http://127.0.0.1:3031")
  "Base URL of the stancer-proxy Node service (no trailing slash).")


(defvar *current-user* nil
  "Currently logged user (plist with :|clientID| :|prenom| :|nom| :|email| :|telephone|).")

;; ── Anti-spam très simple (pour visiteurs non connectés) ─────────────────────
(defparameter *antispam-qa*
  '(("Écrivez « livre »"     . "livre")
    ("Combien font 2 + 3 ?"  . "5")
    ("Tapez « papier »"      . "papier")
    ("Écrivez « roman »"     . "roman")))

(defun pick-antispam ()
  "Retourne (values question answer) depuis *antispam-qa*."
  (let* ((qa (nth (random (length *antispam-qa*)) *antispam-qa*)))
    (values (car qa) (string-downcase (cdr qa)))))

;; Paiement carte (stub) — stockage en mémoire d'un checkout en cours
(defvar *pending-checkouts* (make-hash-table :test 'equal))

(defun %gen-checkout-ref ()
  (string-downcase (princ-to-string (uuid:make-v4-uuid))))

(defun begin-card-checkout (&key ids cards name email phone message
                                 ;; on mémorise l’adresse + mode + shipping
                                 adresse-id l1 l2 ville cp pays mode
                                 shipping-id shipping-label shipping-price)
  "Crée une session 'carte' en mémoire et renvoie une ref."
  (let* ((ref (%gen-checkout-ref))
         (total (total-command cards))
         (payload (list :ids ids
                        :cards cards
                        :name name
                        :email email
                        :phone phone
                        :message message
                        :total total
                        :mode mode
                        ;; adresse
                        :adresse-id adresse-id
                        :l1 l1 :l2 l2 :ville ville :cp cp :pays pays
                        ;; shipping choisi (centimes pour :shipping-price)
                        :shipping_id shipping-id
                        :shipping_label shipping-label
                        :shipping_price shipping-price
                        :created (local-time:now))))
    (setf (gethash ref *pending-checkouts*) payload)
    ref))




(defun get-pending-checkout (ref)
  (and (non-empty-string-p ref) (gethash ref *pending-checkouts*)))

(defun clear-pending-checkout (ref)
  (when (non-empty-string-p ref) (remhash ref *pending-checkouts*)))


;;; ──────────────────────────────────────────────────────────────
;;; Panier: stockage par compte / session (clé en mémoire)
;;; ──────────────────────────────────────────────────────────────

(defvar *carts* (make-hash-table :test 'equal))

;; Sommes-nous dans une requête Hunchentoot ?
(defun in-request-p ()
  (and (boundp 'hunchentoot:*request*)
       hunchentoot:*request*))

(defparameter *bootstrap-sid* "::bootstrap::")

(defun %ensure-session-id ()
  "SID depuis cookie si on est dans une requête ; sinon valeur bootstrap.
   NE TOUCHE PAS AUX COOKIES hors requête (compilation/init)."
  (if (in-request-p)
      (or (hunchentoot:cookie-in "sid")
          (let ((sid (string-downcase (princ-to-string (uuid:make-v4-uuid)))))
            ;; IMPORTANT : set-cookie prend :value
            (hunchentoot:set-cookie "sid" :value sid :path "/" :max-age (* 60 60 24 30))
            sid))
      *bootstrap-sid*))


(defun parse-ids-str (ids-str)
  (parse-ids ids-str)) ;; défini ailleurs



(defun set-cart-ids (ids)
  "Écrase le panier courant (mémoire + cookie) et, si connecté, persiste aussi en DB."
  (let* ((k (current-cart-key))
         (v (remove-duplicates ids :from-end t)))
    (setf (gethash k *carts*) v)
    (%cookie-out! *cart-cookie-name* (cart-ids->str v))
    (when *current-user*
      (let ((cid (getf *current-user* :|clientID|)))
        (db-write-user-cart cid v)))
    v))

(defun clear-cart ()
  "Vide panier mémoire + cookie + DB pour l'utilisateur connecté."
  (dolist (k (remove nil (list (current-cart-key)
                               (session-cart-key)
                               (user-cart-key))))
    (remhash k *carts*))
  (hunchentoot:set-cookie *cart-cookie-name*
    :value "" :path "/" :max-age 0 :expires (- (get-universal-time) 3600))
  (when *current-user*
    (let ((cid (getf *current-user* :|clientID|)))
      (db-clear-user-cart cid)))
  nil)

(defun get-cart-ids ()
  "Récupère la liste d’IDs du panier.
   - si *current-user* → lit la DB ; fallback mémo/cookie ; puis sync mémo+cookie.
   - sinon → mémo ; fallback cookie."
  (let* ((cid (and *current-user* (getf *current-user* :|clientID|))))
    (cond
      (cid
       (let* ((mem (gethash (current-cart-key) *carts*))
              (db  (db-read-user-cart cid))
              (cook (%cookie-in *cart-cookie-name*))
              (co   (and cook (parse-ids cook)))
              (res  (or db mem co '())))
         (setf (gethash (current-cart-key) *carts*) res)
         (%cookie-out! *cart-cookie-name* (cart-ids->str res))
         res))
      (t
       (let* ((mem (gethash (current-cart-key) *carts*))
              (cook (%cookie-in *cart-cookie-name*)))
         (or mem
             (when cook
               (let ((v (parse-ids cook)))
                 (setf (gethash (current-cart-key) *carts*) v)
                 v))
             '()))))))




(defun cart-ids->str (&optional (ids (get-cart-ids)))
  (format nil "~{~a~^,~}" ids))

(defun merge-carts-on-login! ()
  "Fusionne panier de session, cookie, DB vers le panier user au moment du login."
  (let* ((sid (session-cart-key))
         (uid (user-cart-key))
         (sids (gethash sid *carts*))
         (co   (and (%cookie-in *cart-cookie-name*)
                    (parse-ids (%cookie-in *cart-cookie-name*))))
         (cid  (and *current-user* (getf *current-user* :|clientID|)))
         (db   (and cid (db-read-user-cart cid)))
         (merged (remove-duplicates (append db sids co) :from-end t)))
    (when uid
      (setf (gethash uid *carts*) merged)
      (remhash sid *carts*)
      (%cookie-out! *cart-cookie-name* (cart-ids->str merged))
      (when cid (db-write-user-cart cid merged)))))


;; Dans la route de connexion (POST) après authent OK et *current-user* rempli :
;; (merge-carts-on-login!)


;;
;; ────────────────────────────────────────────────────────────────────────────
;; SQLite: connection + schema (commande.db)
;; ────────────────────────────────────────────────────────────────────────────
;;

(defvar *db-path* "commande.db"
  "Path to the SQLite database file.")

(defvar *db-conn* nil
  "SQLite connection object (opened during start).")

(defun %pragma-foreign-keys! ()
  (ignore-errors
    (sqlite:execute-non-query *db-conn* "PRAGMA foreign_keys = ON")))

(defun ensure-db-schema ()
  "Create all required tables if they don't exist."
  (%pragma-foreign-keys!)
  ;; users table
  (sqlite:execute-non-query
   *db-conn*
   "CREATE TABLE IF NOT EXISTS client (
      clientID    INTEGER PRIMARY KEY AUTOINCREMENT,
      prenom      TEXT,
      nom         TEXT,
      email       TEXT UNIQUE,
      telephone   TEXT,
      mdp         TEXT
    )")
  ;; addresses
  (sqlite:execute-non-query
   *db-conn*
   "CREATE TABLE IF NOT EXISTS adresse (
      adresseID   INTEGER PRIMARY KEY AUTOINCREMENT,
      clientID    INTEGER,
      ligne1      TEXT,
      ligne2      TEXT,
      ville       TEXT,
      code_postal TEXT,
      pays        TEXT,
      adresse_complete TEXT DEFAULT '',
      FOREIGN KEY(clientID) REFERENCES client(clientID)
    )")
  ;; orders
  (sqlite:execute-non-query
   *db-conn*
   "CREATE TABLE IF NOT EXISTS commande (
      commandeID    INTEGER PRIMARY KEY AUTOINCREMENT,
      clientID      INTEGER,
      date_commande TEXT,
      prix_total    REAL,
      FOREIGN KEY(clientID) REFERENCES client(clientID)
    )")
  ;; order lines
  (sqlite:execute-non-query
   *db-conn*
   "CREATE TABLE IF NOT EXISTS ligne_commande (
      ligneID       INTEGER PRIMARY KEY AUTOINCREMENT,
      commandeID    INTEGER,
      produitID     INTEGER,
      quantite      INTEGER,
      prix_unitaire REAL,
      FOREIGN KEY(commandeID) REFERENCES commande(commandeID)
    )")
  (sqlite:execute-non-query
   *db-conn*
   "CREATE INDEX IF NOT EXISTS idx_client_email ON client(email)"))

(defun %have-col-p (row-colname wanted)
  (string= (string-downcase row-colname) (string-downcase wanted)))

(defun ensure-adresse-columns! ()
  "Ajoute les colonnes manquantes dans 'adresse' et l'index unique de dédup."
  (let* ((cols
          (mapcar (lambda (row)
                    (etypecase row
                      (vector (aref row 1))
                      (list   (nth 1 row))))
                  (sqlite:execute-to-list *db-conn* "PRAGMA table_info('adresse')"))))
    (flet ((have (c) (find c cols
                           :test (lambda (wanted got)
                                   (string-equal wanted got)))))
      (unless (have "ligne1")
        (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse ADD COLUMN ligne1 TEXT"))
      (unless (have "ligne2")
        (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse ADD COLUMN ligne2 TEXT"))
      (unless (have "ville")
        (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse ADD COLUMN ville TEXT"))
      (unless (have "code_postal")
        (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse ADD COLUMN code_postal TEXT"))
      (unless (have "pays")
        (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse ADD COLUMN pays TEXT"))
      (unless (have "adresse_complete")
        (sqlite:execute-non-query *db-conn*
          "ALTER TABLE adresse ADD COLUMN adresse_complete TEXT DEFAULT ''")))
    (ignore-errors
      (sqlite:execute-non-query
       *db-conn*
       "CREATE UNIQUE INDEX IF NOT EXISTS uniq_adresse_norm ON adresse
        ( clientID,
          lower(trim(ifnull(ligne1,''))),
          lower(trim(ifnull(ligne2,''))),
          lower(trim(ifnull(ville,''))),
          replace(lower(trim(ifnull(code_postal,''))),' ',''),
          lower(trim(ifnull(pays,''))) )"))
    (ignore-errors
      (sqlite:execute-non-query
       *db-conn*
       "UPDATE adresse
           SET adresse_complete =
                 trim(
                   ifnull(ligne1,'') ||
                   case when ifnull(ligne2,'')<>'' then char(10)||ligne2 else '' end ||
                   char(10) || ifnull(code_postal,'') || ' ' || ifnull(ville,'') ||
                   case when ifnull(pays,'')<>'' then char(10)||pays else '' end
                 )
         WHERE adresse_complete IS NULL OR adresse_complete = ''"))
    t))

(defun ensure-adresse-cp-text! ()
  "Assure que adresse.code_postal est en TEXT. Migration robuste :
   - supporte les anciennes bases sans colonne adresseID (on réutilise rowid)
   - supporte un état interrompu (adresse_old déjà présent)
   - recrée l’index unique."
  (labels
      ((table-exists-p (name)
         (plusp (length (sqlite:execute-to-list
                         *db-conn*
                         "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
                         name))))
       (col-type (table col)
         (let* ((rows (sqlite:execute-to-list *db-conn* (format nil "PRAGMA table_info('~a')" table)))
                (cell (find col rows
                            :test (lambda (wanted row)
                                    (string-equal wanted (etypecase row
                                                           (vector (aref row 1))
                                                           (list   (nth 1 row))))))))
           (and cell (etypecase cell
                       (vector (aref cell 2))
                       (list   (nth 2 cell))))))
       (have-col-p (table col)
         (not (null (col-type table col)))))
    (when (table-exists-p "adresse")
      (let ((ctype (col-type "adresse" "code_postal")))
        (unless (and ctype (string-equal (string-upcase ctype) "TEXT"))
          (log:info "[migrate] adresse.code_postal ~a -> TEXT" ctype)
          ;; Si une migration a planté, on peut avoir deja adresse_old et/ou une table adresse vide.
          (let* ((have-old (table-exists-p "adresse_old")))
            ;; 1) Si pas encore renomme, on renomme l’ancienne table en adresse_old
            (unless have-old
              (sqlite:execute-non-query *db-conn* "ALTER TABLE adresse RENAME TO adresse_old"))
            ;; 2) (re)créer la nouvelle table adresse avec la bonne définition
            (unless (table-exists-p "adresse")
              (sqlite:execute-non-query *db-conn*
                "CREATE TABLE adresse (
                   adresseID       INTEGER PRIMARY KEY AUTOINCREMENT,
                   clientID        INTEGER,
                   ligne1          TEXT,
                   ligne2          TEXT,
                   ville           TEXT,
                   code_postal     TEXT,
                   pays            TEXT,
                   adresse_complete TEXT DEFAULT '',
                   FOREIGN KEY(clientID) REFERENCES client(clientID)
                 )"))
            ;; 3) Copier les données depuis adresse_old → adresse
            ;;    - on prend rowid comme adresseID (anciennes bases n'avaient pas la colonne)
            ;;    - on cast code_postal en TEXT
            ;;    - on protège adresse_complete si absente dans l’ancienne table
            (let* ((old-has-complete (have-col-p "adresse_old" "adresse_complete")))
              (sqlite:execute-non-query *db-conn*
                (format nil
                        "INSERT INTO adresse (adresseID, clientID, ligne1, ligne2, ville, code_postal, pays, adresse_complete)
                         SELECT rowid, clientID, ligne1, ligne2, ville,
                                CAST(code_postal AS TEXT), pays,
                                ~a
                           FROM adresse_old"
                        (if old-has-complete "adresse_complete" "''"))))
            ;; 4) Supprimer l’ancienne table si tout est OK
            (sqlite:execute-non-query *db-conn* "DROP TABLE adresse_old")
            ;; 5) Recréer l’index unique
            (ignore-errors
              (sqlite:execute-non-query
               *db-conn*
               "CREATE UNIQUE INDEX IF NOT EXISTS uniq_adresse_norm ON adresse
                ( clientID,
                  lower(trim(ifnull(ligne1,''))),
                  lower(trim(ifnull(ligne2,''))),
                  lower(trim(ifnull(ville,''))),
                  replace(lower(trim(ifnull(code_postal,''))),' ',''),
                  lower(trim(ifnull(pays,''))) )"))
            t))))))



;;; ──────────────────────────────────────────────────────────────
;;; PANIER PERSISTANT EN DB (user_cart)
;;; ──────────────────────────────────────────────────────────────

(defun ensure-user-cart-table! ()
  "Crée la table user_cart si elle n'existe pas."
  (sqlite:execute-non-query
   *db-conn*
   "CREATE TABLE IF NOT EXISTS user_cart (
      clientID   INTEGER PRIMARY KEY,
      ids        TEXT,
      updated_at TEXT,
      FOREIGN KEY(clientID) REFERENCES client(clientID)
    )"))

;; Appelée depuis db-init :
;; (ensure-db-schema) puis (ensure-adresse-columns!) puis >>> (ensure-user-cart-table!)

(defun db-read-user-cart (client-id)
  "Retourne une liste d’IDs (fixnums) depuis la table user_cart pour client-id."
  (handler-case
      (let ((row (db-first-row "SELECT ids FROM user_cart WHERE clientID=?" client-id)))
        (when row
          (let ((ids-str (row-at row 0)))
            (parse-ids ids-str))))
    (error (e)
      (log:error "[db-read-user-cart] ~a" e)
      nil)))

(defun db-write-user-cart (client-id ids-list)
  "Persiste la liste d’IDs pour cet utilisateur. Écrase l’existant.
   Compatible avec SQLite sans `changes`."
  (let* ((ids-str (cart-ids->csv ids-list)))
    (handler-case
        (progn
          ;; 1) Tente UPSERT (SQLite ≥ 3.24) — si la version ne supporte pas, cela lèvera une erreur.
          (sqlite:execute-non-query
           *db-conn*
           "INSERT INTO user_cart (clientID, ids, updated_at)
            VALUES (?,?,datetime('now'))
            ON CONFLICT(clientID) DO UPDATE SET ids=excluded.ids, updated_at=datetime('now')"
           client-id ids-str))
      (error (e)
        ;; 2) Fallback : ON CONFLICT non supporté → faire UPDATE ou INSERT selon existence.
        (declare (ignore e))
        (handler-case
            (let ((row (db-first-row
                        "SELECT clientID FROM user_cart WHERE clientID=?" client-id)))
              (if row
                  ;; row existe -> UPDATE
                  (sqlite:execute-non-query
                   *db-conn*
                   "UPDATE user_cart SET ids=?, updated_at=datetime('now')
                    WHERE clientID=?"
                   ids-str client-id)
                  ;; sinon -> INSERT
                  (sqlite:execute-non-query
                   *db-conn*
                   "INSERT INTO user_cart (clientID, ids, updated_at)
                    VALUES (?,?,datetime('now'))"
                   client-id ids-str)))
          (error (e2)
            (log:error "[db-write-user-cart fallback] ~a" e2)))))))


(defun db-clear-user-cart (client-id)
  "Vide le panier en DB pour cet utilisateur."
  (handler-case
      (sqlite:execute-non-query
       *db-conn*
       "UPDATE user_cart SET ids='', updated_at=datetime('now') WHERE clientID=?" client-id)
    (error (e)
      (log:error "[db-clear-user-cart] ~a" e))))

(defun db-init ()
  "Open the SQLite connection and ensure schema exists."
  (handler-case
      (progn
        (setf *db-conn* (sqlite:connect *db-path*))
        (ensure-db-schema)
        (ensure-adresse-columns!)
        (ensure-adresse-cp-text!)
        (ensure-user-cart-table!)      
        (log:info "[db] SQLite connected: ~A" *db-path*))
    (error (e)
      (log:error "DATABASE ERROR: ~a" e)
      (error "Failed to initialize database"))))


;; Helper: fetch first row safely (as vector or list)
(defun db-first-row (sql &rest params)
  "Return the first row for SQL (or NIL). Uses execute-to-list."
  (first (apply #'sqlite:execute-to-list *db-conn* sql params)))

(defun row-at (row i)
  (etypecase row
    (vector (aref row i))
    (list   (nth i row))))

(defun row->ligne-plist (row)
  (list :|ligneID|       (row-at row 0)
        :|commandeID|    (row-at row 1)
        :|produitID|     (row-at row 2)  
        :|quantite|      (row-at row 3)
        :|prix_unitaire| (row-at row 4)))



(defun row->client-plist (row &key (with-mdp t))
  (if with-mdp
      (list :|clientID|  (row-at row 0)
            :|prenom|    (row-at row 1)
            :|nom|       (row-at row 2)
            :|email|     (row-at row 3)
            :|telephone| (row-at row 4)
            :|mdp|       (row-at row 5))
      (list :|clientID|  (row-at row 0)
            :|prenom|    (row-at row 1)
            :|nom|       (row-at row 2)
            :|email|     (row-at row 3)
            :|telephone| (row-at row 4))))

(defun row->adresse-plist (row)
  (labels ((S (x) (strip-faux-vide x)))
    (list :id          (row-at row 0)
          :client-id   (row-at row 1)
          :ligne1      (S (row-at row 2))
          :ligne2      (S (row-at row 3))
          :ville       (S (row-at row 4))
          :code_postal (S (row-at row 5))
          ;; important: toujours ISO2 pour l’UI et les intégrations
          :pays        (%country->iso2 (S (row-at row 6))))))



(defun find-user-by-email (email)
  "Return a plist of the user matching email, or NIL."
  (handler-case
      (let ((row (db-first-row
                  "SELECT clientID, prenom, nom, email, telephone, mdp
                     FROM client WHERE email = ?" email)))
        (when row (row->client-plist row)))
    (error (e)
      (log:error "DB ERROR in find-user-by-email: ~a" e)
      nil)))

(defun insert-user (prenom nom telephone email mdp)
  "Insert a new client. Returns T on success, NIL on failure."
  (handler-case
      (progn
        (sqlite:execute-non-query
         *db-conn*
         "INSERT INTO client (prenom, nom, email, telephone, mdp)
          VALUES (?, ?, ?, ?, ?)"
         prenom nom email telephone mdp)
        t)
    (error (e)
      (log:error "DB ERROR in insert-user: ~a" e)
      nil)))

(defun check-login (email mdp)
  "Return a plist of user (without mdp) when credentials are correct; else NIL."
  (handler-case
      (let ((row (db-first-row
                  "SELECT clientID, prenom, nom, email, telephone
                     FROM client WHERE email = ? AND mdp = ?"
                  email mdp)))
        (when row
          (row->client-plist row :with-mdp nil)))
    (error (e)
      (log:error "DB ERROR in check-login: ~a" e)
      nil)))

(defun find-client-by-id (id)
  "Return a client (plist) by id."
  (let ((row (db-first-row
              "SELECT clientID, prenom, nom, email, telephone
                 FROM client WHERE clientID = ?" id)))
    (when row (row->client-plist row :with-mdp nil))))

(defun get-adresses-by-client (id)
  (mapcar #'row->adresse-plist
          (sqlite:execute-to-list
           *db-conn*
           "SELECT rowid AS adresseID, clientID, ligne1, ligne2, ville, code_postal, pays
              FROM adresse WHERE clientID = ?"
           id)))

(defun get-commandes-by-client (id)
  ;; On force l'ordre des colonnes pour être parfaitement aligné avec format-order.
  (sqlite:execute-to-list
   *db-conn*
   "SELECT commandeID, clientID, date_commande, prix_total
      FROM commande
     WHERE clientID = ?
  ORDER BY date_commande DESC"
   id))


(defun get-produits-by-commande (commande-id)
  (sqlite:execute-to-list
   *db-conn*
   "SELECT * FROM ligne_commande WHERE commandeID = ?" commande-id))


(defun ensure-guest-client-id ()
  "Retourne l'ID d'un client 'Invité' (créé au besoin)."
  (unless *db-conn* (db-init))
  (let* ((email "guest@abstock.invalid")
         (row (db-first-row "SELECT clientID FROM client WHERE email = ?" email)))
    (if row
        (row-at row 0)
        (progn
          (sqlite:execute-non-query
           *db-conn*
           "INSERT INTO client (prenom, nom, email, telephone, mdp)
            VALUES (?, ?, ?, ?, ?)"
           "Invité" "Site" email "" "")
          (sqlite:last-insert-rowid *db-conn*)))))

;;;; ============================================================
;;;; Helpers : parse ids & créer la commande en DB
;;;; ============================================================

(defun safe-parse-integer (s)
  "Parse un entier de manière permissive, NIL sinon."
  (handler-case
      (when (and s (stringp s) (plusp (length s)))
        (parse-integer s :junk-allowed t))
    (error () nil)))

(defun parse-ids (ids-str)
  "Transforme une chaîne '1,2, 03' en liste de fixnums uniques et triés."
  (let* ((parts (when ids-str (str:split "," ids-str :omit-nulls t)))
         (nums  (remove nil (mapcar (lambda (p)
                                      (let ((n (safe-parse-integer (str:trim p))))
                                        (when (and n (>= n 0)) n)))
                                    parts))))
    (sort (remove-duplicates nums :test #'=) #'<)))

(defun cards-from-ids (ids-str)
  "Retourne la liste de cartes (produits) correspondant aux ids.
   Essaie successivement : FILTER-CARDS-BY-IDS, GET-CARDS-BY-IDS, GET-CARD."
  (let* ((ids (parse-ids ids-str)))
    (cond
      ((and (fboundp 'filter-cards-by-ids) (not (null ids)))
       (funcall 'filter-cards-by-ids ids))
      ((and (fboundp 'get-cards-by-ids) (not (null ids)))
       (funcall 'get-cards-by-ids ids))
      ((and (fboundp 'get-card) (not (null ids)))
       (remove nil (mapcar (lambda (i) (funcall 'get-card i)) ids)))
      (t
       (error "Aucune fonction pour récupérer les produits par id n'est disponible.")))))

(defun normalize-card (c)
  "Retourne une plist normalisée (:id :qty :price :title) depuis l'objet carte c."
  (labels ((pick (&rest keys)
             (loop for k in keys thereis (getf c k))))
    (let* ((id    (or (pick :|id| :id :|produitID| :produit-id) 0))
           (qty   (or (pick :|qty| :qty :|quantite| :quantite) 1))
           (price (or (pick :|price| :price :|prix| :prix :|prix_unitaire| :prix-unitaire) 0))
           (tit   (or (pick :|title| :title :|nom| :nom :|libelle| :libelle) "")))
      (list :id id :qty (or qty 1) :price (or price 0) :title (or tit "")))))

(defun compute-cards-total (cards)
  "Somme en centimes. Si TOTAL-COMMAND existe déjà, on l'utilise."
  (if (fboundp 'total-command)
      (funcall 'total-command cards)
      (reduce #'+ (mapcar (lambda (c)
                            (let* ((nc (normalize-card c))
                                   (q  (getf nc :qty))
                                   (p  (getf nc :price)))
                              (* (or q 1) (or p 0))))
                          cards)
              :initial-value 0)))

(defun create-commande-with-lines (client-id cards
                                   &key email shipping-method payment-method
                                        adresse-id l1 l2 ville cp pays
                                        shipping-cents) ;; <<< ajouté
  (declare (ignore email shipping-method payment-method))
  (unless *db-conn* (db-init))
  (let* ((cid* (or client-id (ensure-guest-client-id)))  ;; jamais NIL
         (total-cards (compute-cards-total cards))
         (total (+ total-cards (or shipping-cents 0)))   ;; <<< prend en compte le port
         (has-adresse-col (table-has-column-p "commande" "adresseID"))
         (addr-id (or adresse-id
                      (ensure-adresse-id! nil cid* l1 l2 ville cp pays))))
    (when (and has-adresse-col (null addr-id))
      (setf addr-id
            (create-adresse cid*
                            (or l1 "Retrait en magasin")
                            (or l2 "")
                            (or ville "")
                            (or cp "")
                            (or pays ""))))
    (cond
      (has-adresse-col
       (sqlite:execute-non-query
        *db-conn*
        "INSERT INTO commande (clientID, adresseID, date_commande, prix_total)
         VALUES (?, ?, datetime('now'), ?)"
        cid* addr-id total))
      (t
       (sqlite:execute-non-query
        *db-conn*
        "INSERT INTO commande (clientID, date_commande, prix_total)
         VALUES (?, datetime('now'), ?)"
        cid* total)))
    (let* ((order-id (sqlite:last-insert-rowid *db-conn*))
           (prod-col (%lc-produit-col)))
      (dolist (c cards)
        (let* ((nc   (normalize-card c))
               (pid  (%value-for-produit-col c prod-col))
               (qty  (or (getf nc :qty) 1))
               (unit (or (getf nc :price) 0)))
          (sqlite:execute-non-query
           *db-conn*
           (format nil
                   "INSERT INTO ligne_commande (commandeID, ~a, quantite, prix_unitaire)
                    VALUES (?, ?, ?, ?)" prod-col)
           order-id pid qty unit)))
      order-id)))


;;; ------------------------------------------
;;; Trouver carte par ISBN / par ID
;;; ------------------------------------------
(defun find-card-by-isbn (isbn)
  (when (and isbn *cards*)
    (find (string isbn) *cards*
          :test #'string= :key (lambda (c) (or (getf c :|isbn|) (getf c :isbn))))))

(defun find-card-by-id (id)
  (when id
    (cond
      ((fboundp 'filter-cards-by-ids)
       (first (filter-cards-by-ids (list id))))
      ;; sinon get-card
      ((fboundp 'get-card)
       (funcall 'get-card id))
      (t nil))))

(defun %lc-produit-col ()
  "Retourne le vrai nom de la colonne produit dans `ligne_commande`.
   On gère maintenant `isbn_produit` (ou `isbn`)."
  (let* ((rows (sqlite:execute-to-list *db-conn* "PRAGMA table_info('ligne_commande')"))
         (cols (mapcar (lambda (row)
                         (let ((nm (etypecase row
                                     (vector (aref row 1))
                                     (list   (nth 1 row)))))
                           (cons (string-downcase (string nm)) nm)))
                       rows))
         (prefer '("isbn_produit" "isbn"         ;; priorité aux ISBN
                   "produitid" "produit_id" "id_produit" "produit"
                   "productid" "product_id" "item_id"
                   "book_id" "card_id" "article_id" "articleid")))
    (or (loop for p in prefer
              for cell = (assoc p cols :test #'string=)
              when cell do (return (cdr cell)))
        (cdr (first cols)))))  ;; fallback minimal

(defun %value-for-produit-col (card produit-col)
  "Si la colonne contient 'isbn' → renvoie ISBN, sinon l'ID."
  (let* ((name (string-downcase (princ-to-string produit-col)))
         (isbn (or (getf card :|isbn|) (getf card :isbn)))
         (id   (or (getf card :|id|)   (getf card :id))))
    (if (search "isbn" name) (or isbn "") id)))

(defun order-lines->display (raw-lines)
  "Transforme les lignes sqlite → plists prêts pour Djula : title, cover, url, subtotal…"
  (let* ((prod-col (%lc-produit-col))
         (is-isbn  (search "isbn" (string-downcase (princ-to-string prod-col)))))
    (mapcar
     (lambda (row)
       (let* ((pl   (row->ligne-plist row))
              (pid  (getf pl :|produitID|))
              (qty  (or (getf pl :|quantite|) 1))
              (unit (or (getf pl :|prix_unitaire|) 0))
              (card (if is-isbn
                        (find-card-by-isbn (princ-to-string pid))
                        (let ((n (and pid (ignore-errors
                                            (if (integerp pid) pid
                                                (parse-integer (princ-to-string pid)))))))
                          (find-card-by-id n))))
              (title (or (and card (getf card :|title|))
                         (princ-to-string pid)))
              (cover (and card (getf card :|cover|)))
              (url   (and card
                          (format nil "/~a/~a-~a"
                                  *card-page-url-name*
                                  (getf card :|id|)
                                  (slug:slugify (getf card :|title|)))))
              (subtotal (* qty unit)))
         (list :|ligneID|       (getf pl :|ligneID|)
               :|produitID|     pid
               :|quantite|      qty
               :|prix_unitaire| unit
               :title title :cover cover :url url :subtotal subtotal)))
     raw-lines)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Adresse helpers (CRUD)
;;; ──────────────────────────────────────────────────────────────────────────

(defun adresse-complete-string (l1 l2 ville code-postal pays)
  (str:trim (format nil "~a~@[~%~a~]~%~a ~a~@[~%~a~]"
                    l1 (and (str:non-blank-string-p l2) l2)
                    (or code-postal "") (or ville "")
                    (and (str:non-blank-string-p pays) pays))))

(defun create-adresse (client-id ligne1 ligne2 ville code-postal pays)
  "Cree l'adresse si elle n'existe pas deja. Retourne deux valeurs:
   ROWID (entier) et le statut :created ou :duplicate."
  (unless *db-conn* (db-init))
  (let* ((L1 (strip-faux-vide ligne1))
         (L2 (strip-faux-vide ligne2))
         (V  (strip-faux-vide ville))
         (CP (strip-faux-vide code-postal))
         (P  (strip-faux-vide pays))
         (nL1 (string-downcase (str:trim L1)))
         (nL2 (string-downcase (str:trim L2)))
         (nV  (string-downcase (str:trim V)))
         (nCP (string-downcase (str:replace-all " " "" (str:trim CP))))
         (nP  (string-downcase (str:trim P))))
    (labels ((find-existing ()
               (db-first-row
                "SELECT rowid
                   FROM adresse
                  WHERE clientID = ?
                    AND lower(trim(ifnull(ligne1,''))) = ?
                    AND lower(trim(ifnull(ligne2,''))) = ?
                    AND lower(trim(ifnull(ville,'')))  = ?
                    AND replace(lower(trim(ifnull(code_postal,''))),' ','') = ?
                    AND lower(trim(ifnull(pays,'')))   = ?"
                client-id nL1 nL2 nV nCP nP)))
      (let ((row (find-existing)))
        (when row
          (return-from create-adresse (values (row-at row 0) :duplicate))))
      (let* ((have-complete (table-has-column-p "adresse" "adresse_complete"))
             (complete      (adresse-complete-string L1 L2 V CP P)))
        (handler-case
            (progn
              (if have-complete
                  (sqlite:execute-non-query
                   *db-conn*
                   "INSERT INTO adresse (clientID, ligne1, ligne2, ville, code_postal, pays, adresse_complete)
                    VALUES (?, ?, ?, ?, ?, ?, ?)"
                   client-id L1 L2 V CP P complete)
                  (sqlite:execute-non-query
                   *db-conn*
                   "INSERT INTO adresse (clientID, ligne1, ligne2, ville, code_postal, pays)
                    VALUES (?, ?, ?, ?, ?, ?)"
                   client-id L1 L2 V CP P))
              (values (sqlite:last-insert-rowid *db-conn*) :created))
          (sqlite:sqlite-constraint-error (c)
            (let ((r (find-existing)))
              (when r
                (return-from create-adresse (values (row-at r 0) :duplicate))))
            (error c)))))))



(defun update-adresse (adresse-id client-id l1 l2 ville code-postal pays)
  "Met à jour une adresse. Retourne :ok ou :duplicate si l’index unique déclenche."
  (unless *db-conn* (db-init))
  (let* ((L1 (strip-faux-vide l1))
         (L2 (strip-faux-vide l2))
         (V  (strip-faux-vide ville))
         (CP (strip-faux-vide code-postal))
         (P  (strip-faux-vide pays))
         (have-complete (table-has-column-p "adresse" "adresse_complete"))
         (complete (adresse-complete-string L1 L2 V CP P)))
    (handler-case
        (progn
          (if have-complete
              (sqlite:execute-non-query
               *db-conn*
               "UPDATE adresse
                  SET ligne1=?, ligne2=?, ville=?, code_postal=?, pays=?, adresse_complete=?
                WHERE rowid=? AND clientID=?"
               L1 L2 V CP P complete adresse-id client-id)
              (sqlite:execute-non-query
               *db-conn*
               "UPDATE adresse
                  SET ligne1=?, ligne2=?, ville=?, code_postal=?, pays=?
                WHERE rowid=? AND clientID=?"
               L1 L2 V CP P adresse-id client-id))
          :ok)
      ;; Collision sur l’index unique → on signale :duplicate pour que la route gère le message
      (sqlite:sqlite-constraint-error (c)
        (declare (ignore c))
        :duplicate))))


(defun delete-adresse (adresse-id client-id)
  (sqlite:execute-non-query
   *db-conn*
   "DELETE FROM adresse WHERE rowid = ? AND clientID = ?"
   adresse-id client-id))

(defun get-adresse (adresse-id client-id)
  (let ((row (first (sqlite:execute-to-list
                     *db-conn*
                     "SELECT rowid AS adresseID, clientID, ligne1, ligne2, ville, code_postal, pays
                        FROM adresse
                       WHERE rowid = ? AND clientID = ?"
                     adresse-id client-id))))
    (when row (row->adresse-plist row))))

;; --- Panier: cookies ---
(defparameter *cart-cookie-name* "cart_ids")

(defun %cookie-in (name)
  (hunchentoot:cookie-in name))

(defun %cookie-out! (name value &key (path "/") (max-age 2592000))
  "max-age ~30 jours."
  (hunchentoot:set-cookie name
                          :value (or value "")
                          :path path
                          :max-age max-age))

(defun cart-ids->csv (ids)
  (if (null ids) "" (format nil "~{~a~^,~}" ids)))


(defun session-cart-key ()
  (format nil "sid:~a" (%ensure-session-id)))  

(defun user-cart-key ()
  (when *current-user*
    (format nil "user:~a" (getf *current-user* :|clientID|))))

(defun current-cart-key ()
  (or (user-cart-key) (session-cart-key)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Commande helpers (detail)
;;; ──────────────────────────────────────────────────────────────────────────

(defun find-user-address-by-id (client-id adresse-id)
  (get-adresse adresse-id client-id))

(defun table-has-column-p (table col)
  (let* ((rows (sqlite:execute-to-list *db-conn* (format nil "PRAGMA table_info('~a')" table))))
    (find col rows
          :test (lambda (wanted row)
                  (string-equal wanted (etypecase row
                                         (vector (aref row 1))
                                         (list   (nth 1 row))))))))

(defun ensure-adresse-id! (adresseID client-id l1 l2 ville cp pays)
  "Retourne un entier adresseID :
   - si `adresseID` fourni → parse et renvoie
   - sinon, si on a une adresse libre (l1…) → on l'insère et on renvoie son rowid
   - sinon → NIL"
  (cond
    ((str:non-blank-string-p adresseID)
     (ignore-errors (parse-integer adresseID)))
    ((str:non-blank-string-p l1)
     (create-adresse client-id l1 l2 ville cp pays))
    (t nil)))

(defun get-commande (commande-id)
  (first (sqlite:execute-to-list
          *db-conn*
          "SELECT commandeID, clientID, date_commande, prix_total
             FROM commande
            WHERE commandeID = ?" commande-id)))

(defun get-lignes-commande (commande-id)
  "Retourne les lignes d'une commande, avec alias SQL de la colonne produit → PRODUITID."
  (let ((col (%lc-produit-col)))
    (sqlite:execute-to-list
     *db-conn*
     (format nil
             "SELECT ligneID, commandeID, ~a AS produitID, quantite, prix_unitaire
                FROM ligne_commande
               WHERE commandeID = ?" col)
     commande-id)))



(defun ensure-owned-commande (commande-id client-id)
  "Return the commande row if it belongs to client-id, else NIL."
  (first (sqlite:execute-to-list
          *db-conn*
          "SELECT commandeID, clientID, date_commande, prix_total
             FROM commande
            WHERE commandeID = ? AND clientID = ?"
          commande-id client-id)))

;;; ──────────────────────────────────────────────────────────────────────────
;;; Routes: Adresses (new/edit/delete)
;;; ──────────────────────────────────────────────────────────────────────────

(easy-routes:defroute adresse-new-get ("/mon-compte/adresses/nouvelle" :method :get) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (djula:render-template* +address-form.html+ nil
        :mode "new"
        :current-user *current-user*
        :user-content *user-content*)))

(easy-routes:defroute adresse-new-post ("/mon-compte/adresses/nouvelle" :method :post) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid   (getf *current-user* :|clientID|))
             (p     (hunchentoot:post-parameters*))
             (l1    (cdr (assoc "ligne1"      p :test #'string=)))
             (l2    (cdr (assoc "ligne2"      p :test #'string=)))
             (ville (cdr (assoc "ville"       p :test #'string=)))
             (cp    (cdr (assoc "code_postal" p :test #'string=)))
             (pays  (cdr (assoc "pays"        p :test #'string=))))
        (unless *db-conn* (db-init))
        (cond
          ((str:blankp l1)
           (djula:render-template* +address-form.html+ nil
             :mode "new"
             :error "La ligne d'adresse est obligatoire."
             :current-user *current-user* :user-content *user-content*
             :ligne1 l1 :ligne2 l2 :ville ville :code_postal cp :pays pays))
          (t
           (handler-case
               (progn
                 (create-adresse cid l1 l2 ville cp pays)
                 (hunchentoot:redirect "/mon-compte"))
             (error (e)
               (log:error "[adresse-new-post] create failed: ~a" e)
               (setf (hunchentoot:return-code*) 400)
               (djula:render-template* +address-form.html+ nil
                 :mode "new"
                 :error (format nil "Erreur DB: ~a" e)
                 :current-user *current-user* :user-content *user-content*
                 :ligne1 l1 :ligne2 l2 :ville ville :code_postal cp :pays pays))))))))

;; GET edit
(easy-routes:defroute adresse-edit-get ("/mon-compte/adresses/:id/editer" :method :get) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid  (getf *current-user* :|clientID|))
             (aid  (ignore-errors (parse-integer id)))
             (addr (and aid (get-adresse aid cid))))
        (if addr
            (djula:render-template* +address-form.html+ nil
              :mode "edit"
              :adresse addr
              :ligne1 (getf addr :ligne1)
              :ligne2 (getf addr :ligne2)
              :ville  (getf addr :ville)
              :code_postal (getf addr :code_postal)
              :pays   (getf addr :pays)
              :current-user *current-user*
              :user-content *user-content*)
            (hunchentoot:redirect "/mon-compte")))))

;; POST edit
(easy-routes:defroute adresse-edit-post ("/mon-compte/adresses/:id/editer" :method :post) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid   (getf *current-user* :|clientID|))
             (aid   (ignore-errors (parse-integer id)))
             (p     (hunchentoot:post-parameters*))
             (l1    (cdr (assoc "ligne1"      p :test #'string=)))
             (l2    (cdr (assoc "ligne2"      p :test #'string=)))
             (ville (cdr (assoc "ville"       p :test #'string=)))
             (cp    (cdr (assoc "code_postal" p :test #'string=)))
             (pays  (cdr (assoc "pays"        p :test #'string=)))
             (addr  (and aid (get-adresse aid cid))))
        (cond
          ((null addr)
           (hunchentoot:redirect "/mon-compte"))
          ((str:blankp l1)
           (djula:render-template* +address-form.html+ nil
             :mode "edit"
             :adresse (list :id aid :ligne1 l1 :ligne2 l2 :ville ville :code_postal cp :pays pays)
             :error "La ligne d'adresse est obligatoire."
             :current-user *current-user* :user-content *user-content*))
          (t
           (update-adresse aid cid l1 l2 ville cp pays)
           (hunchentoot:redirect "/mon-compte"))))))

;; POST delete
(easy-routes:defroute adresse-delete-post ("/mon-compte/adresses/:id/supprimer" :method :post) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid (getf *current-user* :|clientID|))
             (aid (ignore-errors (parse-integer id))))
        (when aid (delete-adresse aid cid))
        (hunchentoot:redirect "/mon-compte"))))


;;; ──────────────────────────────────────────────────────────────
;;; API Panier (add/remove/clear/get)
;;; ──────────────────────────────────────────────────────────────

(defun %json-ok (&key (extra ""))
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (format nil "{\"ok\":true~a}" extra))

(defun %json-cart ()
  (let* ((ids (get-cart-ids)))
    (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
    (format nil "{\"ids\":[~{~a~^,~}],\"count\":~d}" ids (length ids))))

(easy-routes:defroute api-cart-get ("/api/cart" :method :get) ()
  (%json-cart))

(easy-routes:defroute api-cart-add ("/api/cart/add" :method :post) (&post id)
  (declare (ignore id))
  (let* ((nid (%int-from-any "id" "productId" "bookId"))
         (ref (hunchentoot:header-in* :referer))
         (idsq (or (qs-param-from-url ref "ids") ""))
         (url-mode (and idsq (not (string= idsq "")))))
    (if url-mode
        ;; --- MODE URL ---
        (let* ((vis (parse-ids idsq))
               (new (remove-duplicates (append vis (list nid)) :test #'=)))
          (%json-cart/ids new))
        ;; --- MODE PANIER SERVEUR ---
        (let* ((before (get-cart-ids))
               (after  (remove-duplicates (cons nid before) :test #'=)))
          (set-cart-ids after)
          (%json-cart/ids after)))))


;; Supprimer un item sans relire get-cart-ids.
(easy-routes:defroute api-cart-remove ("/api/cart/remove" :method :post) (&post id)
  (declare (ignore id))
  (let* ((nid (%int-from-any "id" "productId" "bookId"))
         (ref (hunchentoot:header-in* :referer))
         (idsq (or (qs-param-from-url ref "ids") ""))
         (url-mode (and idsq (not (string= idsq "")))))
    (if url-mode
        ;; --- MODE URL (?ids=) ---
        (let* ((vis (parse-ids idsq))
               (new (remove nid vis :test #'=)))
          (%json-cart/ids new))
        ;; --- MODE PANIER SERVEUR ---
        (let* ((before (get-cart-ids))
               (after  (remove nid before :test #'=)))
          (set-cart-ids after)
          (%json-cart/ids after)))))



;; Vider totalement le panier (mémoire + cookie), puis répondre count=0.
(easy-routes:defroute api-cart-clear ("/api/cart/clear" :method :post) ()
  (let* ((ref (hunchentoot:header-in* :referer))
         (idsq (or (qs-param-from-url ref "ids") ""))
         (url-mode (and idsq (not (string= idsq "")))))
    (if url-mode
        ;; --- MODE URL : on ne vide pas la mémoire, on renvoie liste vide ---
        (%json-cart/ids '())
        ;; --- MODE PANIER SERVEUR : on vide mémoire + cookie ---
        (progn
          (clear-cart)
          (%json-cart/ids '())))))


;;; ──────────────────────────────────────────────────────────────────────────
;;; Route: Détail d'une commande
;;; ──────────────────────────────────────────────────────────────────────────

(defun format-order (row)
  "Plist pour Djula : :|id| :|date| :|items_count| :|total| :|currency| :|status|
   et aussi les versions :id :date :items_count :total :currency :status."
  (let ((id   (etypecase row (vector (aref row 0)) (list (nth 0 row))))
        (cid  (etypecase row (vector (aref row 1)) (list (nth 1 row))))
        (date (etypecase row (vector (aref row 2)) (list (nth 2 row))))
        (tot  (etypecase row (vector (aref row 3)) (list (nth 3 row)))))
    (declare (ignore cid))
    (let* ((items (length (get-produits-by-commande id)))
           (cur   (abstock/currencies:default-currency-symbol)))
      (list
       ;; clés avec barres
       :|id| id :|date| date :|items_count| items :|total| tot :|currency| cur :|status| nil
       ;; et les versions "simples"
       :id id  :date date  :items_count items  :total tot  :currency cur  :status nil))))



(easy-routes:defroute commande-show ("/commande/:id" :method :get) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid (getf *current-user* :|clientID|))
             (raw (and id (ensure-owned-commande (parse-integer id) cid))))
        (if (null raw)
            (djula:render-template* +404.html+ nil)
            (let* ((order  (format-order raw))
                   (cmd-id (getf order :|id|))
                   (rows   (get-lignes-commande cmd-id))
                   (articles (order-lines->display rows)))
              (djula:render-template* +order-details.html+ nil
                :current-user *current-user*
                :commande order
                ;; on passe "articles", et le template retombe sur "lignes" sinon
                :articles articles
                :user-content *user-content*))))))



;;
;; ────────────────────────────────────────────────────────────────────────────
;; Utils
;; ────────────────────────────────────────────────────────────────────────────
;;

(defparameter *user-template-path/welcome*
  (ensure-directories-exist
   (asdf:system-relative-pathname
    :abstock "static/user/templates/welcome.txt"))
  "Template modified from the admin.")

(defparameter *user-template-path/selection-presentation*
  (asdf:system-relative-pathname
   :abstock "static/user/templates/selection-presentation.txt")
  "Template modified from the admin.")

(defparameter *user-template-path/body*
  (asdf:system-relative-pathname
   :abstock "static/user/templates/body.txt")
  "Template modified from the admin.")

;; Be robust if format-phone-number isn't defined elsewhere in the app.
(unless (fboundp 'format-phone-number)
  (defun format-phone-number (s) (or s "")))

(defun get-template (template &optional (theme *theme*))
  "Loads template from the base templates directory or from the given theme templates directory if it exists."
  (if (and (str:non-blank-string-p theme)
           (probe-file (asdf:system-relative-pathname "abstock" (str:concat "src/templates/themes/" theme "/" template))))
      (str:concat "themes/" theme "/" template)
      template))

(defun get-cards ()
  (if *dev-mode*
      (progn
        (format t "-- *dev-mode* activated: use a small subset of the DB.")
        (subseq *cards* 0 (min 50 (length *cards*))))
      *cards*))


;;;; ==== Utils adresse (affichage robuste) ===================

(defun %nonblank (s)
  "Vrai si s, converti en texte, n’est pas vide (gère aussi les entiers, etc.)."
  (let ((txt (cond
               ((stringp s) s)
               ((null s)    "")
               (t           (princ-to-string s)))))
    (plusp (length (str:trim txt)))))

(defun %addr->txt (l1 l2 ville cp pays)
  (with-output-to-string (s)
    (when (%nonblank l1)   (format s "~a~%" l1))
    (when (%nonblank l2)   (format s "~a~%" l2))
    (when (or (%nonblank cp) (%nonblank ville))
      (format s "~a ~a~%" (or cp "") (or ville "")))
    (when (%nonblank pays) (format s "~a" pays))))

(defun %choice->relay-addr (choice)
  "Extrait l'adresse du relais depuis le tarif choisi, si présente.
   Retourne 5 values: l1 l2 ville cp pays (pays en ISO2 si possible)."
  (labels ((pick (&rest ks)
             (loop for k in ks
                   for v = (getf choice k)
                   when (and v (not (str:blankp (princ-to-string v))))
                   do (return v))))
    ;; l1, l2, ville, cp, pays
    (values
     (pick :relay_addr1 :relay_address1 :relay_address :point_addr1 :point_address1 :address1)
     (pick :relay_addr2 :relay_address2 :point_addr2 :point_address2 :address2)
     (pick :relay_ville :relay_city :point_city :city)
     (pick :relay_cp :relay_zip :relay_postcode :point_cp :zip :cp)
     (pick :relay_country :point_country :country :countryCode))))

(defun %apply-relay-override (choice L1 L2 VIL CP PAY)
  "Si CHOICE décrit un relais, remplace l’adresse (L1..PAY) par celle du relais.
   PAY est normalisé via %country->iso2 si fourni côté relais."
  (multiple-value-bind (r1 r2 rvil rcp rcountry)
      (%choice->relay-addr choice)
    (values (or r1 L1)
            (or r2 L2)
            (or rvil VIL)
            (or rcp CP)
            (%country->iso2 (or rcountry PAY)))))

(defun %addr-hydrate (adresse-id l1 l2 ville cp pays)
  "Hydrate les champs d’adresse à partir d’un adresseID (string **ou** entier)
   si certains champs sont manquants."
  (let* ((cid    (and *current-user* (getf *current-user* :|clientID|)))
         ;; adresse-id peut être un entier (Easy Routes) → on le convertit proprement
         (aid-raw (and adresse-id (princ-to-string adresse-id)))
         (aid     (and (%nonblank aid-raw) (ignore-errors (parse-integer aid-raw))))
         (addr    (and aid cid (get-adresse aid cid))))
    (values
      (or l1    (and addr (getf addr :ligne1))      "")
      (or l2    (and addr (getf addr :ligne2))      "")
      (or ville (and addr (getf addr :ville))       "")
      (or cp    (and addr (getf addr :code_postal)) "")
      (or pays  (and addr (getf addr :pays))        ""))))


;; ===== Shipping helper (utilisé aux étapes paiement/POST) =====

(defun %read-json-body ()
  "Parse JSON body si Content-Type est application/json, sans référencer
   des packages inconnus au read-time. Essaie YASON, CL-JSON, JONATHAN, JSOWN,
   dynamiquement via FIND-SYMBOL. Retourne NIL si rien n'est dispo ou parse fail."
  (labels ((maybe-call (fname pkg raw)
             (let* ((pkg-obj (find-package pkg))
                    (sym     (and pkg-obj (find-symbol fname pkg-obj))))
               (when (and sym (fboundp sym))
                 (funcall sym raw)))))
    (let ((ct (or (hunchentoot:header-in* :content-type) "")))
      (when (search "application/json" ct :test #'char-equal)
        (handler-case
            (let ((raw (hunchentoot:raw-post-data)))
              (or
               (maybe-call "PARSE" "YASON" raw)
               (maybe-call "DECODE-JSON-FROM-STRING" "CL-JSON" raw)
               (maybe-call "PARSE" "JONATHAN" raw)
               (maybe-call "PARSE" "JSOWN" raw)
               nil))
          (error () nil))))))


(defun %parse-urlencoded-body ()
  "Parse un body url-encodé (id=123&x=y) quelle que soit la Content-Type.
   Retourne un alist ((\"id\" . \"123\") …) ou NIL si body vide."
  (let ((raw (hunchentoot:raw-post-data)))
    (when (and raw (plusp (length raw)))
      (loop for pair in (str:split "&" raw :omit-nulls t)
            for kv = (str:split "=" pair)
            for k = (and (first kv) (hunchentoot:url-decode (first kv)))
            for v = (and (second kv) (hunchentoot:url-decode (second kv)))
            when k collect (cons k v)))))


(defun %json-get (obj key)
  "Get KEY from a parsed JSON object across common representations."
  (cond
    ((hash-table-p obj)       (or (gethash key obj) (gethash (string key) obj)))
    ((listp obj)
     (or (cdr (assoc key obj :test #'string=))
         (cdr (assoc (string key) obj :test #'string=))
         (getf obj (intern (string-upcase key) :keyword))))
    (t nil)))

(defun %int-from-any (&rest names)
  "Essaie de lire un entier depuis:
   - query/form params (hunchentoot:parameter)
   - JSON body (application/json)
   - raw body url-encoded, même si le Content-Type est mauvais
   - headers (au cas où on ait un X-Id ou autre)"

  ;; 1) QUERY / FORM (urlencoded OK avec bon content-type)
  (or
   (loop for n in names
         for v = (hunchentoot:parameter n)
         for i = (and v (ignore-errors (parse-integer v)))
         when i do (return i))

   ;; 2) JSON
   (let* ((j (%read-json-body)))
     (when j
       (loop for n in names
             for v = (%json-get j n)
             for i = (cond
                       ((integerp v) v)
                       ((and v (stringp v)) (ignore-errors (parse-integer v)))
                       (t nil))
             when i do (return i))))

   ;; 3) RAW URLENCODED BODY (même si Content-Type = text/plain par erreur)
   (let ((alist (%parse-urlencoded-body)))
     (when alist
       (loop for n in names
             for v = (cdr (assoc n alist :test #'string=))
             for i = (and v (ignore-errors (parse-integer v)))
             when i do (return i))))

   ;; 4) Headers (rare, mais why not)
   (loop for n in names
         for h1 = (hunchentoot:header-in* (string-downcase n))
         for h2 = (hunchentoot:header-in* (string-upcase n))
         for i = (or (and h1 (ignore-errors (parse-integer h1)))
                     (and h2 (ignore-errors (parse-integer h2))))
         when i do (return i))))


;;; --- Normalisation & faux vides --------------------------------------------

(defun strip-faux-vide (s)
  "Transforme '' / \"\" / nil / null en chaîne vide, sinon TRIM normal."
  (let ((x (and s (str:trim (princ-to-string s)))))
    (if (or (null x) (zerop (length x))
            (string= x "''") (string= x "\"\"")
            (string-equal x "nil") (string-equal x "null"))
        ""
        x)))

(defun collapse-ws (s)
  (with-output-to-string (out)
    (let ((prev-space nil))
      (loop for ch across (or s "")
            do (if (find ch '(#\Space #\Tab #\Newline #\Return))
                   (unless prev-space
                     (write-char #\Space out)
                     (setf prev-space t))
                   (progn
                     (write-char ch out)
                     (setf prev-space nil)))))))

(defun norm-part (s &key (lower t) (collapse t))
  (let ((x (strip-faux-vide s)))
    (when collapse (setf x (collapse-ws x)))
    (setf x (str:trim x))
    (when lower (setf x (string-downcase x)))
    x))

(defun norm-cp (cp)
  ;; Code postal sans espaces internes, en minuscules
  (let* ((x (norm-part cp :lower t)))
    (str:replace-all " " "" x)))

(defun %country->iso2 (p)
  "Normalise un libellé de pays libre vers ISO-2 pour les API transporteurs."
  (let* ((s (string-upcase (strip-faux-vide p))))
    (cond
      ((or (string= s "FR") (string= s "FRANCE")) "FR")
      ((or (string= s "BE") (string= s "BELGIQUE")) "BE")
      ((or (string= s "CH") (string= s "SUISSE") (string= s "SWITZERLAND")) "CH")
      ((= (length s) 2) s)
      ;; défaut FR
      (t "FR"))))


;; ──────────────────────────────────────────────────────────────
;; HTTP helpers (Dexador preferred, Drakma fallback) + site base URL
;; ──────────────────────────────────────────────────────────────
(defun %http-post-json (url payload)
  "POST JSON, return decoded JSON."
  (let ((json (cl-json:encode-json-to-string payload)))
    (labels ((try-dexador ()
               (let* ((pkg (find-package :dexador))
                      (post (and pkg (find-symbol "POST" pkg))))
                 (when (and pkg post)
                   (multiple-value-bind (body _status _headers)
                       (funcall post url
                                :headers '(("Content-Type" . "application/json"))
                                :content json)
                     (declare (ignore _status _headers))
                     (cl-json:decode-json-from-string body)))))
             (try-drakma ()
               (let* ((pkg (find-package :drakma))
                      (http (and pkg (find-symbol "HTTP-REQUEST" pkg))))
                 (when (and pkg http)
                   (multiple-value-bind (body _code _hdrs)
                       (funcall http url
                                :method :post
                                :content json
                                :content-type "application/json"
                                :accept "application/json")
                     (declare (ignore _code _hdrs))
                     (cl-json:decode-json-from-string body))))))
      (or (try-dexador) (try-drakma)
          (error "Neither Dexador nor Drakma is available for HTTP POST.")))))

(defun %http-get-json (url)
  "GET JSON, return decoded JSON."
  (labels ((try-dexador ()
             (let* ((pkg (find-package :dexador))
                    (get (and pkg (find-symbol "GET" pkg))))
               (when (and pkg get)
                 (multiple-value-bind (body _status _headers)
                     (funcall get url :headers '(("Accept" . "application/json")))
                   (declare (ignore _status _headers))
                   (cl-json:decode-json-from-string body)))))
           (try-drakma ()
             (let* ((pkg (find-package :drakma))
                    (http (and pkg (find-symbol "HTTP-REQUEST" pkg))))
               (when (and pkg http)
                 (multiple-value-bind (body _code _hdrs)
                     (funcall http url :method :get :accept "application/json")
                   (declare (ignore _code _hdrs))
                   (cl-json:decode-json-from-string body))))))
    (or (try-dexador) (try-drakma)
        (error "Neither Dexador nor Drakma is available for HTTP GET."))))

(defun %site-base-url ()
  "Best-effort base URL (proxy aware)."
  (let* ((xfp (or (hunchentoot:header-in* "x-forwarded-proto") ""))
         (proto (cond
                  ((string-equal xfp "https") "https")
                  ((string-equal xfp "http")  "http")
                  (t (if (hunchentoot:ssl-p) "https" "http"))))
         (host (or (hunchentoot:header-in* "x-forwarded-host")
                   (hunchentoot:header-in* "host")
                   (format nil "127.0.0.1:~a" *port*))))
    (format nil "~a://~a" proto host)))

;;
;; ────────────────────────────────────────────────────────────────────────────
;; Templates
;; ────────────────────────────────────────────────────────────────────────────
;;

(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))

(defun load-theme-templates (&optional (theme *theme*))
  "Load the Djula templates of the given theme."
  (when (str:non-blank-string-p theme)
    (format-box t (format nil "Loading theme \"~a\" !" theme))
    (djula:add-template-directory
     (asdf:system-relative-pathname
      "abstock" (str:concat "src/templates/themes/" theme "/")))))

(defparameter +base.html+                (djula:compile-template* (get-template "base.html" *theme*)))
(defparameter +welcome.html+             (djula:compile-template* (get-template "welcome.html" *theme*)))
(defparameter +cards.html+               (djula:compile-template* (get-template "cards.html" *theme*)))
(defparameter +selection-page.html+      (djula:compile-template* (get-template "selection-page.html" *theme*)))
(defparameter +card-page.html+           (djula:compile-template* (get-template "card-page.html" *theme*)))
(defparameter +admin-page.html+          (djula:compile-template* (get-template "admin.html" *theme*)))
(defparameter +contact.html+             (djula:compile-template* (get-template "contact.html" *theme*)))
(defparameter +connexion.html+           (djula:compile-template* (get-template "connexion.html" *theme*)))
(defparameter +inscription.html+         (djula:compile-template* (get-template "inscription.html" *theme*)))
(defparameter +agenda.html+              (djula:compile-template* (get-template "agenda.html" *theme*)))
(defparameter +panier.html+              (djula:compile-template* (get-template "panier.html" *theme*)))
(defparameter +command-confirmed.html+   (djula:compile-template* (get-template "command-confirmed.html" *theme*)))
(defparameter +error-messages.html+      (djula:compile-template* (get-template "error-messages.html" *theme*)))
(defparameter +404.html+                 (djula:compile-template* (get-template "404.html" *theme*)))
(defparameter +mon-compte.html+          (djula:compile-template* (get-template "mon-compte.html" *theme*)))
;; New templates used by address/profile/commande routes:
(defparameter +address-form.html+        (djula:compile-template* (get-template "address-form.html" *theme*)))
(defparameter +order-details.html+       (djula:compile-template* (get-template "order-details.html" *theme*)))
(defparameter +edit-profile.html+        (djula:compile-template* (get-template "edit-profile.html" *theme*)))
(defparameter +change-password.html+     (djula:compile-template* (get-template "change-password.html" *theme*)))
(defparameter +pay-card-stub.html+       (djula:compile-template* (get-template "pay-card-stub.html" *theme*)))
;; --- Checkout templates (compilés au boot)
(defparameter +checkout-adresse.html+
  (djula:compile-template* (get-template "checkout-adresse.html" *theme*)))
(defparameter +checkout-livraison.html+
  (djula:compile-template* (get-template "checkout-livraison.html" *theme*)))
(defparameter +checkout-paiement.html+
  (djula:compile-template* (get-template "checkout-paiement.html" *theme*)))

(defun load-templates (&optional (theme *theme*))
  (setf
    +base.html+              (djula:compile-template* (get-template "base.html" theme))
    +welcome.html+           (djula:compile-template* (get-template "welcome.html" theme))
    +cards.html+             (djula:compile-template* (get-template "cards.html" theme))
    +selection-page.html+    (djula:compile-template* (get-template "selection-page.html" theme))
    +card-page.html+         (djula:compile-template* (get-template "card-page.html" theme))
    +admin-page.html+        (djula:compile-template* (get-template "admin.html" theme))
    +connexion.html+         (djula:compile-template* (get-template "connexion.html" theme))
    +inscription.html+       (djula:compile-template* (get-template "inscription.html" theme))
    +agenda.html+            (djula:compile-template* (get-template "agenda.html" theme))
    +panier.html+            (djula:compile-template* (get-template "panier.html" theme))
    +command-confirmed.html+ (djula:compile-template* (get-template "command-confirmed.html" theme))
    +error-messages.html+    (djula:compile-template* (get-template "error-messages.html" theme))
    +404.html+               (djula:compile-template* (get-template "404.html" theme))
    +mon-compte.html+        (djula:compile-template* (get-template "mon-compte.html" theme))
    +address-form.html+      (djula:compile-template* (get-template "address-form.html" theme))
    +order-details.html+     (djula:compile-template* (get-template "order-details.html" theme))
    +edit-profile.html+      (djula:compile-template* (get-template "edit-profile.html" theme))
    +change-password.html+   (djula:compile-template* (get-template "change-password.html" theme))
    +pay-card-stub.html+     (djula:compile-template* (get-template "pay-card-stub.html" theme))
    +checkout-adresse.html+  (djula:compile-template* (get-template "checkout-adresse.html"  theme))
    +checkout-livraison.html+ (djula:compile-template* (get-template "checkout-livraison.html" theme))
    +checkout-paiement.html+  (djula:compile-template* (get-template "checkout-paiement.html"  theme))))


;;
;; ────────────────────────────────────────────────────────────────────────────
;; Djula filters
;; ────────────────────────────────────────────────────────────────────────────
;;

(djula:def-filter :price (val)
  (format nil "~a" (abstock/currencies:display-price val)))

(djula:def-filter :rest (list) (rest list))
(djula:def-filter :slugify (title) (slug:slugify title))

(djula:def-filter :url (card)
  (format nil "/~a/~a-~a"
          *card-page-url-name*
          (getf card :|id|)
          (slug:slugify (getf card :|title|))))

(setf djula::*elision-string* "…")


;;
;; ────────────────────────────────────────────────────────────────────────────
;; Static assets
;; ────────────────────────────────────────────────────────────────────────────
;;


(defparameter *default-static-directory* "src/static/"
  "Directory to serve static assets from. Assets are under /static/.")

(defun serve-static-assets ()
  (let* ((dispatcher
           (hunchentoot:create-folder-dispatcher-and-handler
            "/static/"
            (merge-pathnames *default-static-directory*
                             (asdf:system-source-directory :abstock)))))
    (push (lambda (req)
            (let ((h (funcall dispatcher req))) ; h = handler ou NIL
              (when h
                (lambda ()
                  (setf (hunchentoot:header-out :cache-control)
                        "public, max-age=31536000, immutable")
                  (funcall h)))))
          hunchentoot:*dispatch-table*)))



(hunchentoot:define-easy-handler (favicon :uri "/favicon.ico") ()
  (hunchentoot:redirect "/static/img/owl_login.png"))


;;
;; ────────────────────────────────────────────────────────────────────────────
;; User editable texts
;; ────────────────────────────────────────────────────────────────────────────
;;

(defun read-custom-file (path)
  (when (uiop:file-exists-p (ensure-directories-exist path))
    (str:from-file path)))

(defun get-user-custom-texts ()
  (if *use-admin-custom-texts*
      (bt:with-lock-held (*user-template-lock*)
        (list
         (list :id :welcome
               :title "Présentation de la librairie"
               :content (read-custom-file *user-template-path/welcome*))
         (list :id :selection
               :title "Présentation de la sélection du libraire"
               :content (read-custom-file *user-template-path/selection-presentation*))
         (list :id :body
               :title "Troisième texte"
               :content (read-custom-file *user-template-path/body*))))
      (progn
        (log:info "We are NOT reading the admin content saved on file. See *user-template-path/body*")
        nil)))

;;
;; ────────────────────────────────────────────────────────────────────────────
;; Routes — public pages
;; ────────────────────────────────────────────────────────────────────────────
;;

(easy-routes:defroute root-route ("/" :method :get) ()
  (djula:render-template* +welcome.html+ nil
    :contact *contact-infos*
    :user-content *user-content*
    :user-custom-texts (get-user-custom-texts)
    :current-user *current-user*
    :selection-cards (if (fboundp 'get-selection-subset)
                         (get-selection-subset :ensure-cover t)
                         '())
    :search-form-template (get-template "search-form.html" *theme*)
    :shelves *shelves*))

(easy-routes:defroute selection-route ("/selection-du-libraire" :method :get) ()
  (djula:render-template* +selection-page.html+ nil
    :user-content *user-content*
    :current-user *current-user*
    :selection (if (fboundp 'get-selection) (get-selection) '())
    :shelves *shelves*))

(easy-routes:defroute search-route ("/search" :method :get) (q rayon page)
  (format t "~& /search ~a, rayon: ~a, page: ~a~&" q rayon page)
  (let ((rayon (when rayon (ignore-errors (parse-integer rayon)))))
    (multiple-value-bind (cards result-length isbns-not-found pagination-object)
        (if (fboundp 'search-cards)
            (search-cards (slug:asciify (str:downcase q))
                          :shelf rayon
                          :page page)
            (values '() 0 '() nil))
      (djula:render-template* +cards.html+ nil
        :search-form-template (get-template "search-form.html" *theme*)
        :current-user *current-user*
        :title (format nil "~a - ~a" (user-content-brand-name *user-content*) q)
        :user-content *user-content*
        :query q
        :query-length (length (str:words q))
        :shelf_id rayon
        :cards cards
        :isbns-not-found isbns-not-found
        :length-isbns-not-found (length isbns-not-found)
        :shelves *shelves*
        :current-page 1
        :total-pages (if (fboundp 'get-nb-pages)
                         (get-nb-pages result-length *page-length*)
                         1)
        :pagination pagination-object
        :no-results (or (null result-length) (zerop result-length))))))


(defun %ref-ids ()
  "Retourne la valeur de ?ids= trouvée dans la requête elle-même ou dans le Referer."
  (or (hunchentoot:parameter "ids")
      (qs-param-from-url (hunchentoot:header-in* :referer) "ids")))

(defun %visible-ids ()
  "Liste d'IDs « visibles » :
   - si on est en mode URL (?ids=), on la parse ;
   - sinon, on lit la mémoire/cookie."
  (let ((idsq (%ref-ids)))
    (if (and idsq (not (string= idsq "")))
        (parse-ids idsq)
        (get-cart-ids))))


(defun %json-cart/ids (ids-list)
  "Réponse JSON standard pour le panier : renvoie aussi ids_str pour que le front remplace l'URL."
  (let* ((ids-str (format nil "~{~a~^,~}" (or ids-list '())))
         (count   (length (or ids-list '()))))
    (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
    (format nil "{\"ok\":true,\"count\":~d,\"ids\":[~{~a~^,~}],\"ids_str\":\"~a\"}"
            count ids-list ids-str)))

;; Désactiver totalement l’injection du panier via ?ids=
(defparameter *allow-url-seed?* nil)  

(defun get-visible-cart-ids ()
  "IDs visibles dans le panier :
   - si *allow-url-seed?* et ?ids= présent → on l'utilise (affichage uniquement)
   - sinon, on prend le panier serveur (mémoire/cookie)."
  (let ((ids-param (hunchentoot:parameter "ids")))
    (if (and *allow-url-seed?* ids-param (not (string= ids-param "")))
        (parse-ids ids-param)
        (get-cart-ids))))


;; Désactiver totalement l’injection du panier via ?ids= dans /panier
(easy-routes:defroute panier-route ("/panier" :method :get) (open)
  (declare (ignore open))
  (let* ((ids-list (get-visible-cart-ids))
         (cards    (if (fboundp 'filter-cards-by-ids)
                       (filter-cards-by-ids ids-list)
                       '()))
         (ids-str  (format nil "~{~a~^,~}" ids-list))
         (u        *current-user*)
         (prefill-name  (when u (format nil "~a ~a" (getf u :|prenom|) (getf u :|nom|))))
         (prefill-email (when u (getf u :|email|)))
         (prefill-phone (when u (getf u :|telephone|))))
    (log:info "[/panier GET] key=~a cookie-cart=~s -> ids-list=~s (visible)"
              (current-cart-key) (%cookie-in *cart-cookie-name*) ids-list)
    (djula:render-template* +panier.html+ nil
      :title (format nil "~a - ~a" (user-content-brand-name *user-content*) "Mon Panier")
      :current-user *current-user*
      :cards cards
      :ids ids-str
      :show_validation nil
      :prefill_name  prefill-name
      :prefill_email prefill-email
      :prefill_phone prefill-phone
      :default_currency (abstock/currencies:default-currency-symbol)
      :user-content *user-content*
      :contact *contact-infos*
      :total (total-command cards))))



(easy-routes:defroute robots-route ("/robots.txt" :method :get) ()
  (setf (hunchentoot:content-type*) "text/plain")
  (uiop:read-file-string "robots.txt"))

(easy-routes:defroute panier-count ("/panier/count" :method :get) ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (format nil "~a" (length (get-cart-ids))))  ;; <<< PAS get-visible-cart-ids



;;; ──────────────────────────────────────────────────────────────
;;; Checkout step 1 — Adresse (GET/POST)
;;; ──────────────────────────────────────────────────────────────
(defun checkout-address/impl (ids)
  (let* ((idsq (and ids (str:non-blank-string-p ids) ids))
         (err  (hunchentoot:parameter "err"))
         (error-message (when (string= err "address_exists")
                          "Cette adresse existe deja. Veuillez la selectionner dans la liste ci-dessous.")))
    (if (null *current-user*)
        (djula:render-template* +checkout-adresse.html+ nil
          :current-user *current-user*
          :ids idsq
          :form_error error-message
          :user-content *user-content*)
        (let* ((cid (getf *current-user* :|clientID|))
               (adresses (get-adresses-by-client cid)))
          (djula:render-template* +checkout-adresse.html+ nil
            :current-user *current-user*
            :ids idsq
            :form_error error-message
            :adresses adresses
            :has_adresses (and adresses (> (length adresses) 0))
            :user-content *user-content*)))))


(easy-routes:defroute checkout-address ("/checkout/adresse" :method :get) (ids)
  (if (null *current-user*)
      (checkout-address/impl ids)
      (progn
        (let* ((cid (getf *current-user* :|clientID|))
               (adrs (get-adresses-by-client cid)))
          (log:info "[/checkout/adresse] cid=~a adresses=~a" cid (length adrs)))
        (checkout-address/impl ids))))

(easy-routes:defroute checkout-address-post ("/checkout/adresse" :method :post)
    (&post ids adresseID ligne1 ligne2 ville code_postal pays next mode shipping_id ship)
  (let* ((ids   (or ids (hunchentoot:parameter "ids") ""))
         (next  (or next (hunchentoot:parameter "next")))
         (mode  (or mode (hunchentoot:parameter "mode") "livraison"))
         ;; choix de livraison (radio / widget) capturé ici et propagé
         (sid   (or shipping_id
                    ship
                    (hunchentoot:parameter "shipping_id")
                    (hunchentoot:parameter "ship")))
         (aid   (or adresseID
                    (hunchentoot:parameter "adresseId")
                    (hunchentoot:parameter "existing_address")
                    (hunchentoot:parameter "existing_adresse"))))
    (cond
      ;; 1) adresse existante cochée
      ((and *current-user* (str:non-blank-string-p aid))
       (if (string= next "payment")
           (hunchentoot:redirect
            (concatenate 'string
                         "/checkout/payment"
                         (qs-join `(("ids"        . ,ids)
                                    ("mode"       . ,mode)
                                    ("adresseID"  . ,aid)
                                    ("shipping_id". ,sid)))))
           (hunchentoot:redirect
            (concatenate 'string
                         "/checkout/livraison"
                         (qs-join `(("ids"        . ,ids)
                                    ("adresseID"  . ,aid)
                                    ("shipping_id". ,sid)))))))

      ;; 2) nouvelle adresse saisie
      ((and *current-user* (str:non-blank-string-p ligne1))
       (let* ((cid (getf *current-user* :|clientID|)))
         (multiple-value-bind (new-id status)
             (create-adresse cid ligne1 ligne2 ville code_postal pays)
           (if (eq status :duplicate)
               (hunchentoot:redirect
                (concatenate 'string
                             "/checkout/adresse"
                             (qs-join `(("ids"        . ,ids)
                                        ("next"       . "payment")
                                        ("mode"       . ,mode)
                                        ("shipping_id". ,sid)
                                        ("err"        . "address_exists")))))
               (if (string= next "payment")
                   (hunchentoot:redirect
                    (concatenate 'string
                                 "/checkout/payment"
                                 (qs-join `(("ids"        . ,ids)
                                            ("mode"       . ,mode)
                                            ("adresseID"  . ,new-id)
                                            ("shipping_id". ,sid)))))
                   (hunchentoot:redirect
                    (concatenate 'string
                                 "/checkout/livraison"
                                 (qs-join `(("ids"        . ,ids)
                                            ("adresseID"  . ,new-id)
                                            ("shipping_id". ,sid))))))))))

      ;; 3) invalide -> réaffiche le formulaire
      (t
       (setf (hunchentoot:return-code*) 400)
       (let ((adresses (and *current-user*
                            (get-adresses-by-client (getf *current-user* :|clientID|)))))
         (djula:render-template* +checkout-adresse.html+ nil
           :current-user *current-user*
           :ids ids
           :adresses adresses
           :form_error "Choisis une adresse enregistree ou renseigne au minimum l'adresse (ligne 1), la ville et le code postal."
           :ligne1 ligne1 :ligne2 ligne2 :ville ville :code_postal code_postal :pays pays
           ;; on conserve la sélection de livraison pour réaffichage
           :shipping_id sid
           :user-content *user-content*))))))



;; ── Shipping helpers (Colissimo statique) ─────────────────────


(easy-routes:defroute api-shipping-rates ("/api/shipping/rates" :method :get)
    (ids l1 l2 ville cp pays)
  (let* ((offers (shipping-offers-for ids l1 l2 ville cp pays))
         (arr (mapcar (lambda (o)
                        (list (cons "id"       (getf o :id))
                              (cons "provider" (getf o :provider))
                              (cons "label"    (getf o :label))
                              (cons "eta"      (getf o :eta))
                              (cons "price"    (getf o :price))))
                      offers)))
    (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
    (cl-json:encode-json-to-string arr)))


;;; ──────────────────────────────────────────────────────────────
;;; Checkout step 2 — Livraison (GET/POST)
;;; ──────────────────────────────────────────────────────────────
(easy-routes:defroute checkout-shipping ("/checkout/livraison" :method :get)
    (ids adresseID l1 l2 ville cp pays)
  (let* ((aid-raw (or adresseID
                      (hunchentoot:parameter "adresseID")
                      (hunchentoot:parameter "adresseid")
                      (hunchentoot:parameter "adresseId")))
         ;; on uniformise en string pour %nonblank et l’hydratation
         (aid-str (and aid-raw (princ-to-string aid-raw)))
         (mode   (or (hunchentoot:parameter "mode") "sur_place")))
    (multiple-value-bind (L1 L2 VIL CP PAY)
        (%addr-hydrate aid-str l1 l2 ville cp pays)
      (let* ((addr-present (or (%nonblank L1) (%nonblank L2)
                               (%nonblank VIL) (%nonblank CP) (%nonblank PAY)))
             (has-aid      (%nonblank aid-str))
             (addr-txt     (%addr->txt L1 L2 VIL CP PAY))
             (cards        (ignore-errors (cards-from-ids ids)))
             (shipping-options nil)
             (shipping-error  nil))
        ;; Tarifs de livraison (Colissimo via abstock/shipping) uniquement si mode livraison + adresse présente
        (when (and (string= mode "livraison") addr-present)
          (handler-case
              (setf shipping-options
                    (abstock/shipping:rates-for-address
                     (list :l1 L1 :l2 L2 :ville VIL :cp CP :pays (%country->iso2 PAY))
                     (or cards '())))
            (error (e)
              (setf shipping-error (format nil "~a" e)))))
        (djula:render-template* +checkout-livraison.html+ nil
          :current-user *current-user*
          :ids ids
          :adresseID aid-str
          :l1 L1 :l2 L2 :ville VIL :cp CP :pays PAY
          :addr_present addr-present
          :has_adresseid has-aid
          :adresse_txt addr-txt
          :mode mode
          :mode_sur_place (string= mode "sur_place")
          :mode_livraison (string= mode "livraison")
          ;; ↓ ajouts
          :shipping_options (or shipping-options '())
          :shipping_options_count (length (or shipping-options '()))
          :shipping_error shipping-error
          :user-content *user-content*)))))



;; --- Petits helpers de compatibilité (anciens handlers) ---
(defun current-user-id (req)
  (declare (ignore req))
  (and *current-user* (getf *current-user* :|clientID|)))

(defun param (req key)
  "Récupère un paramètre de requête ou POST."
  (declare (ignore req))
  (hunchentoot:parameter key))

(defun redirect (url)
  (hunchentoot:redirect url))


(defun render-checkout-livraison (req)
  (let* ((ids         (param req "ids"))
         (adresse-id  (param req "adresseID"))
         ;; valeurs déjà présentes (si on revient de l’étape 3 par ex.)
         (l1 (param req "l1")) (l2 (param req "l2"))
         (cp (param req "cp")) (ville (param req "ville")) (pays (param req "pays")))
    ;; Si on n’a que l’ID, on hydrate depuis la DB pour l’affichage
    (when (and (or (null l1) (string= l1 ""))
               (and adresse-id (not (string= adresse-id ""))))
      (let ((addr (find-user-address-by-id (current-user-id req) (parse-integer adresse-id))))
        (when addr
          (setf l1   (getf addr :ligne1)
                l2   (getf addr :ligne2)
                cp   (getf addr :code-postal)
                ville (getf addr :ville)
                pays (or (getf addr :pays) "France")))))
    ;; rendu
    (djula:render-template* +checkout-livraison.html+
      :ids ids :adresseID adresse-id
      :l1 l1 :l2 l2 :cp cp :ville ville :pays pays)))


(easy-routes:defroute checkout-shipping-post ("/checkout/livraison" :method :post) ()
  (let* ((ids   (or (hunchentoot:parameter "ids") ""))
         (mode  (or (hunchentoot:parameter "mode_livraison")
                    (hunchentoot:parameter "mode") "sur_place"))
         (shp   (or (hunchentoot:parameter "shipping_id")
                    (hunchentoot:parameter "ship")))
         (ref   (hunchentoot:header-in* :referer))
         ;; pickup / relais (peuvent arriver via POST ou via l’URL précédente)
         (pid   (or (hunchentoot:parameter "pickup_id")
                    (qs-param-from-url ref "pickup_id")))
         (plab  (or (hunchentoot:parameter "pickup_label")
                    (qs-param-from-url ref "pickup_label")))
         ;; champs d’adresse : on prend ce qu’on a (POST), sinon on retombe sur le referer
         (aid   (or (hunchentoot:parameter "adresseID")
                    (qs-param-from-url ref "adresseID")))
         (L1    (or (hunchentoot:parameter "l1")    (qs-param-from-url ref "l1")))
         (L2    (or (hunchentoot:parameter "l2")    (qs-param-from-url ref "l2")))
         (VIL   (or (hunchentoot:parameter "ville") (qs-param-from-url ref "ville")))
         (CP    (or (hunchentoot:parameter "cp")    (qs-param-from-url ref "cp")))
         (PAY   (or (hunchentoot:parameter "pays")  (qs-param-from-url ref "pays"))))
    (labels ((%nonb (x) (and x (plusp (length (str:trim (princ-to-string x)))))))
      (let ((has-addr (or (%nonb aid) (%nonb L1) (%nonb VIL) (%nonb CP) (%nonb PAY))))
        (cond
          ((string= mode "sur_place")
           (let* ((qs (qs-join `(("ids" . ,ids) ("mode" . ,mode)
                                 ("shipping_id" . ,shp)
                                 ("pickup_id" . ,pid) ("pickup_label" . ,plab)))))
             (hunchentoot:redirect (concatenate 'string "/checkout/payment" qs))))
          ((string= mode "livraison")
           (if has-addr
               (let* ((qs (qs-join `(("ids" . ,ids) ("mode" . ,mode)
                                      ("adresseID" . ,aid)
                                      ("l1" . ,L1) ("l2" . ,L2)
                                      ("ville" . ,VIL) ("cp" . ,CP) ("pays" . ,PAY)
                                      ("shipping_id" . ,shp)
                                      ("pickup_id" . ,pid) ("pickup_label" . ,plab)))))
                 (hunchentoot:redirect (concatenate 'string "/checkout/payment" qs)))
               (let* ((qs (qs-join `(("ids" . ,ids) ("next" . "payment")
                                     ("mode" . ,mode)
                                     ("shipping_id" . ,shp)
                                     ("pickup_id" . ,pid) ("pickup_label" . ,plab)))))
                 (hunchentoot:redirect (concatenate 'string "/checkout/adresse" qs)))))
          (t
           (let* ((qs (qs-join `(("ids" . ,ids) ("mode" . ,mode)
                                  ("adresseID" . ,aid)
                                  ("l1" . ,L1) ("l2" . ,L2)
                                  ("ville" . ,VIL) ("cp" . ,CP) ("pays" . ,PAY)
                                  ("shipping_id" . ,shp)
                                  ("pickup_id" . ,pid) ("pickup_label" . ,plab)))))
             (hunchentoot:redirect (concatenate 'string "/checkout/payment" qs)))))))))


(defun qs-param-from-url (url key)
  "Extrait la valeur du paramètre KEY depuis une URL (string) ou NIL."
  (when (and url (stringp url))
    (let* ((qpos (position #\? url))
           (qs   (and qpos (subseq url (1+ qpos)))))
      (when qs
        (loop for pair in (str:split "&" qs :omit-nulls t)
              for kv = (str:split "=" pair)
              for k = (first kv)
              for v = (second kv)
              when (and k (string= k key))
                do (return (and v (hunchentoot:url-decode v))))))))


(defun qs-join (pairs)
  "Construit une query-string à partir d'un alist ((key . val) ...),
   en ignorant les valeurs vides/nil. Retourne soit \"?k=v&...\" soit \"\"."
  (labels ((nonempty (x)
             (and x (> (length (princ-to-string x)) 0)))
           (enc (s)
             (hunchentoot:url-encode (princ-to-string s))))
    (let* ((parts (loop for (k . v) in pairs
                        when (nonempty v)
                        collect (format nil "~A=~A" (enc k) (enc v))))
           (joined (if parts (format nil "~{~A~^&~}" parts) "")))
      (if (zerop (length joined))
          ""
          (concatenate 'string "?" joined)))))

(defun post-checkout-livraison (req)
  (let* ((ids        (param req "ids"))
         (mode-liv   (param req "mode_livraison"))
         (mode       (if (string= mode-liv "sur_place") "sur_place" "livraison"))
         (adresse-id (or (param req "adresseID") (param req "existing_adresse")))
         (l1 (param req "l1"))
         (l2 (param req "l2"))
         (cp (param req "cp"))
         (ville (param req "ville"))
         (pays (param req "pays")))
    (redirect
     (concatenate 'string
                  "/checkout/payment"
                  (qs-join
                   `(("ids"        . ,ids)
                     ("mode"       . ,mode)
                     ("adresseID"  . ,adresse-id)
                     ("l1" . ,l1) ("l2" . ,l2)
                     ("cp" . ,cp) ("ville" . ,ville) ("pays" . ,pays)
                     ("shipping_id" . ,(or (hunchentoot:parameter "shipping_id")
                                           (hunchentoot:parameter "ship")))))))))


;; ──────────────────────────────────────────────────────────────
;; STANCER integration (via stancer-proxy)
;; ──────────────────────────────────────────────────────────────

(defun stancer-amount-cents (pending)
  "Total en centimes = produits (:total) + port (:shipping_price)."
  (+ (or (getf pending :total) 0)
     (or (getf pending :shipping_price) 0)))

(defun stancer-create-payment! (ref pending &key return-url)
  "POST /pay sur le stancer-proxy. Retourne (:id :url)."
  (let* ((amount (stancer-amount-cents pending))
         (email  (or (getf pending :email) ""))
         (name   (or (getf pending :name)  ""))
         (phone  (or (getf pending :phone) ""))
         (desc   (format nil "ABStock commande ~a" ref))
         (payload `(("amount"     . ,amount)
                    ("currency"   . "EUR")
                    ("reference"  . ,ref)
                    ("email"      . ,email)
                    ("name"       . ,name)
                    ("phone"      . ,phone)
                    ("return_url" . ,return-url)
                    ("description". ,desc))))
    (handler-case
        (let* ((endpoint (str:concat *stancer-proxy-url* "/pay"))
               (res (%http-post-json endpoint payload)))
          (flet ((get (k)
                   (cond
                     ((hash-table-p res) (gethash k res))
                     ((listp res) (cdr (assoc k res :test #'string=))))))
            (let ((pid (or (get "id") (get "payment_id")))
                  (url (or (get "url") (get "payment_url"))))
              (unless (and pid url)
                (error "stancer-proxy: invalid response (~a)" res))
              (list :id pid :url url))))
      (error (e)
        (error "Stancer create error: ~a" e)))))

(defun stancer-fetch-status (payment-id)
  "GET /status?id=PAYMENT_ID — retourne :paid/:authorized/:pending/:canceled/:failed."
  (let* ((endpoint (format nil "~a/status?id=~a" *stancer-proxy-url* payment-id))
         (res (%http-get-json endpoint)))
    (flet ((g (k)
             (cond ((hash-table-p res) (gethash k res))
                   ((listp res) (cdr (assoc k res :test #'string=))))))
      (let* ((status (or (g "status") (g "state") ""))
             (paid?  (or (g "paid") (g "captured") (string= status "captured")))
             (auth?  (or (g "authorized") (string= status "authorized"))))
        (cond
          (paid?       :paid)
          (auth?       :authorized)
          ((string= status "pending")   :pending)
          ((string= status "canceled")  :canceled)
          ((string= status "failed")    :failed)
          (t                            :pending))))))

;; =======================
;; STEP 3 — Paiement (GET)
;; =======================
(easy-routes:defroute checkout-payment ("/checkout/payment" :method :get)
    (ids mode l1 l2 ville cp pays)
  (let* ((aid  (or (hunchentoot:parameter "adresseid")
                   (hunchentoot:parameter "adresseID")
                   (hunchentoot:parameter "adresseId")))
         (mode* (or mode (hunchentoot:parameter "mode") "livraison"))
         (shp  (or (hunchentoot:parameter "shipping_id")
                   (hunchentoot:parameter "ship")))
         ;; ▼ NEW
         (pid  (or (hunchentoot:parameter "pickup_id")
                   (hunchentoot:parameter "pickupId")))
         (plab (hunchentoot:parameter "pickup_label")))
    (when (or (null mode*) (string= mode* "") (string= mode* "''"))
      (setf mode* "livraison"))
    (multiple-value-bind (L1 L2 VIL CP PAY)
        (%addr-hydrate aid l1 l2 ville cp pays)
      (let* ((offers (shipping-offers-for ids L1 L2 VIL CP (%country->iso2 PAY)))
             ;; ▼ Prefer exact relay rate when we have a pickup_id
             (choice (or (and pid (abstock/shipping:find-rate-by-id offers pid))
                         (and shp (abstock/shipping:find-rate-by-id offers shp))))
             (ship-price (or (and choice (getf choice :price)) 0))
             (cards-total (compute-cards-total (cards-from-ids ids)))
             (grand-total (+ cards-total ship-price)))
        (multiple-value-bind (dL1 dL2 dV dCP dP dTXT is-relay relay-label)
            (%relay-choice->addr+txt choice L1 L2 VIL CP PAY)
          (declare (ignore dTXT))
          (djula:render-template* +checkout-paiement.html+ nil
            :current-user *current-user*
            :ids ids :mode mode*
            :mode_sur_place (string= mode* "sur_place")
            :mode_livraison (string= mode* "livraison")
            :adresseID aid
            ;; address becomes the relay address when is-relay=T
            :l1 dL1 :l2 dL2 :ville dV :cp dCP :pays dP
            :is_relay is-relay
            ;; ▼ keep widget label if Djula didn’t get one from the rate
            :pickup_label (or relay-label plab)
            :shipping_locked (str:non-blank-string-p shp)
            :shipping_id (or pid shp)  ;; exposes the precise id to the template/JS
            :shipping_label (or (and choice (getf choice :label)) plab)
            :shipping_price ship-price
            :cards_total cards-total
            :grand_total grand-total
            :user-content *user-content*))))))



;;; ──────────────────────────────────────────────────────────────
;;; Checkout step 3b — Pay (POST)
;;; ──────────────────────────────────────────────────────────────

(easy-routes:defroute checkout-pay-post ("/checkout/pay" :method :post) ()
  (let* ((ids   (or (hunchentoot:parameter "ids") ""))
         (cards (cards-from-ids ids))
         (name  (and *current-user* (format nil "~a ~a"
                                            (getf *current-user* :|prenom|)
                                            (getf *current-user* :|nom|))))
         (email (and *current-user* (getf *current-user* :|email|)))
         (phone (and *current-user* (getf *current-user* :|telephone|)))
         (mode* (or (hunchentoot:parameter "mode")
                    (hunchentoot:parameter "mode_livraison")))
         (paym  (or (hunchentoot:parameter "payment_method")
                    (hunchentoot:parameter "payment")))
         ;; adresse
         (adresseID (or (hunchentoot:parameter "adresseID")
                        (hunchentoot:parameter "adresseId")
                        (hunchentoot:parameter "existing_address")
                        (hunchentoot:parameter "existing_adresse")))
         (l1   (hunchentoot:parameter "l1"))
         (l2   (hunchentoot:parameter "l2"))
         (ville (hunchentoot:parameter "ville"))
         (cp    (hunchentoot:parameter "cp"))
         (pays  (hunchentoot:parameter "pays"))
         ;; ==== shipping choisi ====
         (ship-id (or (hunchentoot:parameter "shipping_id")
                      (hunchentoot:parameter "ship")))
         (offers (shipping-offers-for ids l1 l2 ville cp (%country->iso2 pays)))
         (choice (and ship-id (abstock/shipping:find-rate-by-id offers ship-id)))
         (ship-price (or (and choice (getf choice :price)) 0))
         (ship-label (and choice (getf choice :label))))
    ;; Sur-place interdit si mode ≠ sur_place
    (when (and (string= paym "sur-place")
               (not (string= mode* "sur_place")))
      (return-from checkout-pay-post
        (djula:render-template* +checkout-paiement.html+ nil
          :current-user *current-user*
          :ids ids :adresseID adresseID :mode mode*
          :l1 l1 :l2 l2 :ville ville :cp cp :pays pays
          :form_error "Le paiement sur place n’est possible que pour un retrait sur place."
          :user-content *user-content*)))
    (let* ((cid  (and *current-user* (getf *current-user* :|clientID|)))
           (aid* (ensure-adresse-id! adresseID cid l1 l2 ville cp pays))
           (need-aid (table-has-column-p "commande" "adresseID")))
      ;; Si adresse obligatoire et manquante
      (when (and need-aid (null aid*) (not (string= mode* "sur_place")))
        (return-from checkout-pay-post
          (djula:render-template* +checkout-paiement.html+ nil
            :current-user *current-user*
            :ids ids :adresseID "" :mode mode*
            :l1 l1 :l2 l2 :ville ville :cp cp :pays pays
            :form_error "Aucune adresse fournie. Merci d’en choisir une à l’étape précédente."
            :user-content *user-content*)))
      ;; Paiement carte -> mémorise shipping et redirige
      (when (string= paym "carte")
        (let ((ref (begin-card-checkout
                     :ids ids
                     :cards cards
                     :name name :email email :phone phone :message nil
                     :mode mode*
                     :adresse-id adresseID :l1 l1 :l2 l2 :ville ville :cp cp :pays pays
                     :shipping-id ship-id :shipping-label ship-label :shipping-price ship-price)))
          (return-from checkout-pay-post
            (hunchentoot:redirect (format nil "/paiement/carte?ref=~a" ref)))))
      ;; Sur place -> créer la commande avec le port inclus
      (let* ((order-id (create-commande-with-lines cid cards :shipping-cents ship-price)))
        (clear-cart)
        (djula:render-template* +command-confirmed.html+ nil
          :current-user *current-user*
          :order_id order-id
          :name name :email email :phone phone
          :payment (payment-label "sur-place") :delivery mode*
          :cards cards
          :total (total-command cards)   ;; (s'il faut afficher le TTC, additionne ship-price côté template)
          :user-content *user-content*)))))


;;
;; Basket validation (emails)
;;

(defun cards-to-txt (cards)
  (with-output-to-string (s)
    (loop for card in cards
          do (format s "- ~a; ~a; ~a; ~a~&"
                     (getf card :|title|)
                     (getf card :|author|)
                     (abstock/currencies:display-price (getf card :|price|))
                     (getf card :|isbn|)))))

(defun total-command (cards)
  (reduce #'+ cards :key (lambda (it) (getf it :|price|))))

(defun email-content (name email phone payment delivery message cards)
  (with-output-to-string (s)
    (format s "Bonjour cher libraire,~&~%~&Un nouveau client a commandé des livres.~&~%")
    (format s "Ses coordonnées sont: ~&- ~a~&- mail: ~a ~&- tél: ~a ~%"
            name email (format-phone-number phone))
    (format s "~%Mode de livraison: ~a~&" (or delivery "retrait"))
    (format s "Moyen de paiement: ~a~&~%" (or payment "Sur place"))
    (format s "Il/elle a commandé:~&~%~a~&~%" (cards-to-txt cards))
    (format s "Le total de la commande est de: ~a.~&~%"
            (abstock/currencies:display-price (total-command cards)))
    (when (not (str:blankp message))
      (format s "~%Message client:~%«~a»~%~%" (str:shorten 300 message)))
    (format s "Nous sommes le: ~a~&"
            (local-time:format-timestring nil (local-time:now)
                                          :format local-time:+rfc-1123-format+))
    (format s "À bientôt !~&")))

(defun confirmation-email-content (cards)
  (with-output-to-string (s)
    (format s "Bonjour,~&~%")
    (format s "Merci pour votre commande. Voici le récapitulatif :~&~%")
    (format s "~a~&~%" (cards-to-txt cards))
    (format s "Total : ~a.~&~%"
            (abstock/currencies:display-price (total-command cards)))
    (format s "~%Nous vous tiendrons informé lorsque votre commande sera prête. À bientôt !~&")))

(defun send-confirmation-email (&key to name from reply-to cards)
  (declare (ignorable name))
  (bt:make-thread (lambda ()
                    (email-send :to to
                                :from from
                                :reply-to reply-to
                                :subject "Confirmation de commande"
                                :content (confirmation-email-content cards)))
                  :name "confirmation-email"))

;; Normalisation + libellés paiement
(defun normalize-payment (p)
  (let ((x (str:downcase (or p ""))))
    (cond
      ((or (string= x "sur-place") (string= x "sur place") (string= x "a-definir") (string= x "à-definir")) "sur-place")
      ((string= x "carte") "carte")
      (t "sur-place"))))

(defun payment-label (p)
  (cond
    ((string= p "carte") "Carte (en ligne)")
    ((string= p "sur-place") "Sur place")
    (t p)))

;; ------------------------------------------------------------------
;; POST /panier — validation + confirmation
;; ------------------------------------------------------------------
(easy-routes:defroute panier-validate-route ("/panier" :method :post)
    (&post name email phone payment antispam antispam_expected ids message)
  (let* ((ids-str (or ids ""))
         (ids-list (remove nil (mapcar #'safe-parse-integer
                                       (str:split "," ids-str :omit-nulls t))))
         (cards-db (get-cards))
         (cards (loop for id in ids-list
                      for pos = (position id cards-db :key (lambda (card) (getf card :|id|)))
                      when pos collect (elt cards-db pos)))
         (u *current-user*)
         (prefill-name  (when u (format nil "~a ~a" (getf u :|prenom|) (getf u :|nom|))))
         (prefill-email (when u (getf u :|email|)))
         (prefill-phone (when u (getf u :|telephone|)))
         (retour        (format nil "/panier?open=1"))
         (logged-in-p   (not (null u)))
         (payment (normalize-payment payment)))
    (cond
      ;; 1) Anti-spam requis si pas connecté
      ((and (not logged-in-p)
            (not (string= (str:downcase (or antispam ""))
                          (str:downcase (or antispam_expected "")))))
       (multiple-value-bind (q a) (pick-antispam)
         (djula:render-template* +panier.html+ nil
           :title (format nil "~a - ~a" (user-content-brand-name *user-content*) "Mon Panier")
           :current-user *current-user*
           :cards cards
           :ids ids-str
           :show_validation t
           :validation_form_template (get-template "validation-form.html" *theme*)
           :form-errors (list "La réponse à la question anti-spam est incorrecte.")
           :form-data `(:name ,name :email ,email :phone ,phone :message ,message)
           :prefill_name prefill-name :prefill_email prefill-email :prefill_phone prefill-phone
           :retour_url retour
           :antispam_question q
           :antispam_expected a
           :user-content *user-content*
           :contact *contact-infos*)))

      ;; 2) Besoin d'au moins un moyen de contact
      ((and (str:blankp phone) (str:blankp email))
       (multiple-value-bind (q a)
           (if logged-in-p (values nil nil) (pick-antispam))
         (djula:render-template* +panier.html+ nil
           :title (format nil "~a - ~a" (user-content-brand-name *user-content*) "Mon Panier")
           :current-user *current-user*
           :cards cards
           :ids ids-str
           :show_validation t
           :validation_form_template (get-template "validation-form.html" *theme*)
           :form-errors (list "Veuillez renseigner un email ou un numéro de téléphone.")
           :form-data `(:name ,name :email ,email :phone ,phone :message ,message)
           :prefill_name prefill-name :prefill_email prefill-email :prefill_phone prefill-phone
           :retour_url retour
           :antispam_question q
           :antispam_expected a
           :user-content *user-content*
           :contact *contact-infos*)))

      ;; 3) Paiement carte -> redirection page de paiement (stub)
      ((string= payment "carte")
       (let ((ref (begin-card-checkout :ids ids-str :cards cards
                                       :name name :email email :phone phone
                                       :message message)))
         (hunchentoot:redirect (format nil "/paiement/carte?ref=~a" ref))))

      ;; 4) Sur place -> tente emails puis affiche confirmation (avec création commande)
      (t
        (let ((email-sent-p nil))
          (handler-case
              (progn
                (email-send :to (getf *email-config* :|to|)
                            :reply-to (list email name)
                            :subject "Commande site"
                            :content (email-content name email phone (payment-label payment) nil message cards))
                (when (str:non-blank-string-p email)
                  (send-confirmation-email :to email
                                           :name name
                                           :reply-to (list (getf *email-config* :|to|)
                                                           (user-content-brand-name *user-content*))
                                           :from (getf *email-config* :|from|)
                                           :cards cards))
                (setf email-sent-p t))
            (error (c)
              (log:error "email error (ignored for UX): sending to '~a' with ids '~a' failed: ~a"
                         email ids-str c)))
          (let* ((cid (and *current-user* (getf *current-user* :|clientID|)))
                 (order-id (create-commande-with-lines cid cards)))
            (clear-cart)                 ;; <<< VIDER LE PANIER ICI
            (djula:render-template* +command-confirmed.html+ nil
              :current-user *current-user*
              :order_id order-id
              :name name :email email :phone phone
              :payment (payment-label payment) :delivery nil
              :message message
              :cards cards
              :total (total-command cards)
              :email_sent email-sent-p
              :user-content *user-content*)))))))




;; ================================
;; CARD PAYMENT with STANCER
;; ================================

;; A) Create Stancer session and redirect to hosted payment page
(easy-routes:defroute card-pay ("/paiement/carte" :method :get) (ref)
  (let ((p (get-pending-checkout ref)))
    (if (null p)
        (djula:render-template* +error-messages.html+ nil
          :current-user *current-user*
          :messages (list "Session de paiement introuvable ou expirée.")
          :user-content *user-content*)
        (handler-case
            (let* ((return-url (format nil "~a/paiement/carte/retour?ref=~a"
                                       (%site-base-url) ref))
                   (session (stancer-create-payment! ref p :return-url return-url))
                   (pid (getf session :id))
                   (url (getf session :url)))
              ;; mémoriser l'id du paiement stancer pour le retour
              (setf (getf p :stancer_payment_id) pid)
              (setf (gethash ref *pending-checkouts*) p)
              (hunchentoot:redirect url))
          (error (e)
            (djula:render-template* +error-messages.html+ nil
              :current-user *current-user*
              :messages (list (format nil "Erreur lors de la création du paiement: ~a" e))
              :user-content *user-content*))))))

;; B) Return URL hit by Stancer after customer finishes on hosted page
(easy-routes:defroute card-pay-return ("/paiement/carte/retour" :method :get) (ref id pid payment_id)
  (let* ((p (get-pending-checkout ref))
         (pay-id (or id pid payment_id (and p (getf p :stancer_payment_id)))))
    (cond
      ((or (null p) (null pay-id))
       (djula:render-template* +error-messages.html+ nil
         :current-user *current-user*
         :messages (list "Impossible de valider ce paiement (session perdue).")
         :user-content *user-content*))
      (t
       (handler-case
           (let* ((status (stancer-fetch-status pay-id)))
             (case status
               ((:paid :authorized)
                ;; succès → créer la commande + vider
                (let* ((name   (getf p :name))
                       (email  (getf p :email))
                       (phone  (getf p :phone))
                       (cards  (getf p :cards))
                       (mode   (getf p :mode))
                       (adresse-id (getf p :adresse-id))
                       (l1 (getf p :l1)) (l2 (getf p :l2))
                       (ville (getf p :ville)) (cp (getf p :cp)) (pays (getf p :pays))
                       (ship-cents (or (getf p :shipping_price) 0))
                       (cid (and *current-user* (getf *current-user* :|clientID|)))
                       (order-id (create-commande-with-lines
                                   cid cards
                                   :adresse-id adresse-id
                                   :l1 l1 :l2 l2 :ville ville :cp cp :pays pays
                                   :shipping-cents ship-cents)))
                  (clear-pending-checkout ref)
                  (clear-cart)
                  ;; emails (best-effort)
                  (handler-case
                      (progn
                        (email-send :to (getf *email-config* :|to|)
                                    :reply-to (list email name)
                                    :subject "Commande (paiement carte)"
                                    :content (email-content name email phone (payment-label "carte") mode nil cards))
                        (when (str:non-blank-string-p email)
                          (send-confirmation-email :to email
                                                   :name name
                                                   :reply-to (list (getf *email-config* :|to|)
                                                                   (user-content-brand-name *user-content*))
                                                   :from (getf *email-config* :|from|)
                                                   :cards cards)))
                    (error (c) (log:error "card email error: ~a" c)))
                  (djula:render-template* +command-confirmed.html+ nil
                    :current-user *current-user*
                    :order_id order-id
                    :name name :email email :phone phone
                    :payment (payment-label "carte") :delivery mode
                    :cards cards
                    :total (total-command cards)
                    :user-content *user-content*)))
               ((:pending)
                (djula:render-template* +error-messages.html+ nil
                  :current-user *current-user*
                  :messages (list "Paiement en attente de validation. Réessayez dans un instant ou contactez-nous.")
                  :user-content *user-content*))
               (otherwise
                (djula:render-template* +error-messages.html+ nil
                  :current-user *current-user*
                  :messages (list "Paiement annulé ou refusé. Vous pouvez réessayer.")
                  :user-content *user-content*))))
         (error (e)
           (djula:render-template* +error-messages.html+ nil
             :current-user *current-user*
             :messages (list (format nil "Erreur de validation du paiement: ~a" e))
             :user-content *user-content*)))))))

;; (optional) keep POST confirm compatibility: just bounce to GET
(easy-routes:defroute card-pay-confirm ("/paiement/carte/confirm" :method :post) (&post ref)
  (hunchentoot:redirect (format nil "/paiement/carte?ref=~a" ref)))




(defun get-cards-same-author (card)
  (when card
    (sort
     (filter-cards-by-author (getf card :|author|)
                             :exclude-id (getf card :|id|))
     #+sbcl
     #'sb-unicode:unicode<
     #-sbcl
     (progn
       (log:warn "sorting by unicode string is only supported on SBCL.")
       #'string-lessp)
     :key (lambda (c) (getf c :|title|)))))

(easy-routes:defroute card-page-route (#.*card-page-url* :method :get) ()
  (let* ((card-id (ignore-errors (parse-integer (first (str:split "-" slug)))))
         (card (when card-id (first (filter-cards-by-ids (list card-id)))))
         (same-author (when card (get-cards-same-author card)))
         (same-shelf (when card
                       (pick-cards :n 6
                                   :cards (filter-cards-by-shelf-id (getf card :|shelf_id|))
                                   :shelf-id (getf card :|shelf_id|)
                                   :exclude-id (getf card :|id|)))))
    (cond
      ((null card-id) (djula:render-template* +404.html+ nil))
      (card
       (djula:render-template* +card-page.html+ nil
         :current-user *current-user*
         :card card
         :user-content *user-content*
         :same-author same-author
         :same-shelf same-shelf))
      (t (djula:render-template* +404.html+ nil)))))

(easy-routes:defroute api-summary-route ("/api/card/:id/summary" :method :get) ()
  (when (and (boundp '*fetch-summaries*) *fetch-summaries*)
    (let* ((card (search-card id)))
      (log:info card)
      (unless (str:non-blank-string-p (access card :|summary|))
        (let ((summary (datasources/librairiedeparis::get-summary-from-isbn (access card :|isbn|))))
          (when summary
            (setf (access card :|summary|) summary)
            (setf (hunchentoot:content-type*) "text/plain")
            summary))))))

(easy-routes:defroute random-card ("/au-pif") ()
  (let* ((card (first (pick-cards :n 1)))
         (same-author (get-cards-same-author card)))
    (djula:render-template* +card-page.html+ nil
      :current-user *current-user*
      :card card
      :user-content *user-content*
      :same-author same-author)))

;;
;; ────────────────────────────────────────────────────────────────────────────
;; Admin (custom texts) — returns plain text, no JSON
;; ────────────────────────────────────────────────────────────────────────────
;;

(defun build-uuid ()
  (setf *admin-uuid*
        (uuid:make-v5-uuid
         uuid:+namespace-url+
         (str:concat "abstock"
                     (with-output-to-string (s)
                       (loop repeat 10
                             do (format s "~a" (code-char (+ 80 (random 10))))))))))

(defun build-api-token ()
  (or *api-token*
      (setf *api-token*
            (uuid:make-v5-uuid uuid:+namespace-url+ "abstock"))))

(defun get-admin-url ()
  (or (setf *admin-url*
            (str:downcase
             (str:concat "/"
                         (with-output-to-string (s)
                           (format s "~a" (or *admin-uuid* (build-uuid))))
                         "-admin")))
      *admin-url*))

(hunchentoot:define-easy-handler (admin-route :uri (get-admin-url)) ()
  (let ((txt (read-custom-file *user-template-path/welcome*)))
    (log:info txt)
    (djula:render-template* +admin-page.html+ nil
      :api-token *api-token*
      :user-custom-texts (get-user-custom-texts)
      :user-content *user-content*)))

(defun write-custom-file (textid content)
  "From a textid (either 'welcome', 'selection' or 'body'), write content to the corresponding file."
  (flet ((to-file (path content)
           (str:to-file (ensure-directories-exist path) content)))
    (cond
      ((string-equal textid "welcome")
       (to-file *user-template-path/welcome* content))
      ((string-equal textid "selection")
       (to-file *user-template-path/selection-presentation* content))
      ((string-equal textid "body")
       (to-file *user-template-path/body* content))
      (t
       (log:warn "Writing custom content to file ~S is unknown" textid)))))

;; Plain text admin save (no JSON)
(easy-routes:defroute save-admin-route ("/uuid-admin" :method :post) (textid api-token)
  (declare (ignore api-token))
  (let ((token (or (hunchentoot:header-in* "api-token") "")))
    (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
    (cond
      ((or (null *api-token*)
           (not (string= token *api-token*)))
       (setf (hunchentoot:return-code*) 403)
       "authorization failed.")
      ((not textid)
       (setf (hunchentoot:return-code*) 400)
       "textid is null")
      (t
       (bt:with-lock-held (*user-template-lock*)
         (write-custom-file textid (str:trim (hunchentoot:raw-post-data)))
         (log:info "Template for ~S saved." textid))
       "ok"))))

;;
;; ────────────────────────────────────────────────────────────────────────────
;; Account pages (SQLite) — uses commande.db
;; ────────────────────────────────────────────────────────────────────────────
;;

(easy-routes:defroute contact-route ("/contact" :method :get) ()
  (djula:render-template* +contact.html+ nil
    :current-user *current-user*
    :user-content *user-content*))

(easy-routes:defroute agenda-route ("/agenda" :method :get) ()
  (djula:render-template* +agenda.html+ nil
    :current-user *current-user*
    :user-content *user-content*))

(easy-routes:defroute mon-compte-route ("/mon-compte" :method :get) ()
  (unless *db-conn* (db-init))
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid       (getf *current-user* :|clientID|))
             (client    (or (find-client-by-id cid) *current-user*))
             (adresses  (and client (get-adresses-by-client (getf client :|clientID|))))
             (commandes (and client (get-commandes-by-client (getf client :|clientID|))))
             (orders    (mapcar #'format-order (or commandes '())))
             (profile-ok (and client
                              (str:non-blank-string-p (getf client :|prenom|))
                              (str:non-blank-string-p (getf client :|nom|))
                              (str:non-blank-string-p (getf client :|email|)))))
        (djula:render-template* +mon-compte.html+ nil
          :current-user *current-user*
          :client client
          :adresses (or adresses '())
          :commandes (or commandes '())
          :orders orders
          :default_currency (abstock/currencies:default-currency-symbol)
          :prenom (getf client :|prenom|)
          :nom (getf client :|nom|)
          :email (getf client :|email|)
          :telephone (getf client :|telephone|)
          :profile_ok profile-ok))))

(easy-routes:defroute logout-route ("/logout" :method :get) ()
  (let* ((old-sid (hunchentoot:cookie-in "sid"))
         (user-k  (and *current-user* (user-cart-key))))
    ;; purge both possible carts
    (when user-k (remhash user-k *carts*))
    (when old-sid (remhash (format nil "sid:~a" old-sid) *carts*))
    ;; wipe cart cookie
    (%cookie-out! *cart-cookie-name* "" :max-age 0)
    ;; rotate sid cookie (persistent, like when you issue it originally)
    (hunchentoot:set-cookie "sid"
      :value (string-downcase (princ-to-string (uuid:make-v4-uuid)))
      :path "/"
      :max-age (* 60 60 24 30)))
  (setf *current-user* nil)
  (hunchentoot:redirect "/"))


;; GET /inscription
(easy-routes:defroute inscription-get-route ("/inscription" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/mon-compte")
      (djula:render-template* +inscription.html+ nil
        :current-user *current-user*
        :user-content *user-content*)))

;; POST /inscription
(easy-routes:defroute inscription-post-route ("/inscription" :method :post) ()
  (let* ((params    (hunchentoot:post-parameters*))
         (prenom    (cdr (assoc "prenom" params :test #'string=)))
         (nom       (cdr (assoc "nom" params :test #'string=)))
         (telephone (cdr (assoc "telephone" params :test #'string=)))
         (email     (cdr (assoc "email" params :test #'string=)))
         (mdp       (cdr (assoc "mdp" params :test #'string=)))
         (confirm   (cdr (assoc "confirm_password" params :test #'string=)))
         (existing  (and (str:non-blank-string-p email) (find-user-by-email email))))
    (cond
      ((or (str:blankp prenom) (str:blankp nom) (str:blankp email) (str:blankp mdp))
       (djula:render-template* +inscription.html+ nil
         :current-user *current-user*
         :error "Veuillez remplir tous les champs obligatoires."
         :user-content *user-content*))
      ((not (string= mdp confirm))
       (djula:render-template* +inscription.html+ nil
         :current-user *current-user*
         :error "Les mots de passe ne correspondent pas"
         :user-content *user-content*))
      (existing
       (djula:render-template* +inscription.html+ nil
         :current-user *current-user*
         :error "Cet email est déjà utilisé"
         :user-content *user-content*))
      ((not (insert-user prenom nom telephone email mdp))
       (djula:render-template* +inscription.html+ nil
         :current-user *current-user*
         :error "Une erreur est survenue lors de la création du compte."
         :user-content *user-content*))
      (t
       (setf *current-user* (check-login email mdp)) ;; auto-login
       (hunchentoot:redirect "/mon-compte")))))

;; GET /connexion
(easy-routes:defroute connexion-get-route ("/connexion" :method :get) ()
  (if *current-user*
      (hunchentoot:redirect "/mon-compte")
      (djula:render-template* +connexion.html+ nil
        :user-content *user-content*
        :current-user *current-user*)))

;; POST /connexion
(easy-routes:defroute connexion-post-route ("/connexion" :method :post) ()
  (let* ((params (hunchentoot:post-parameters*))
         (email  (cdr (assoc "email" params :test #'string=)))
         (mdp    (cdr (assoc "mdp"   params :test #'string=)))
         (user   (check-login email mdp)))
    (if user
        (progn
          (setf *current-user* user)
          (merge-carts-on-login!)
          (hunchentoot:redirect "/mon-compte"))
        (djula:render-template* +connexion.html+ nil
          :error "Email ou mot de passe incorrect"
          :user-content *user-content*
          :current-user *current-user*))))

;; -------------------------------
;; Éditer profil
;; -------------------------------

(easy-routes:defroute edit-profile-get ("/mon-compte/editer" :method :get) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let ((client (find-client-by-id (getf *current-user* :|clientID|))))
        (djula:render-template* +edit-profile.html+ nil
          :current-user *current-user*
          :client client
          :user-content *user-content*))))

(easy-routes:defroute edit-profile-post ("/mon-compte/editer" :method :post) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid       (getf *current-user* :|clientID|))
             (params    (hunchentoot:post-parameters*))
             (prenom    (cdr (assoc "prenom" params :test #'string=)))
             (nom       (cdr (assoc "nom" params :test #'string=)))
             (telephone (cdr (assoc "telephone" params :test #'string=)))
             (email     (cdr (assoc "email" params :test #'string=))))
        (cond
          ((or (str:blankp prenom) (str:blankp nom) (str:blankp email))
           (djula:render-template* +edit-profile.html+ nil
             :current-user *current-user*
             :client (find-client-by-id cid)
             :error "Tous les champs obligatoires doivent être remplis."
             :user-content *user-content*))
          (t
           (sqlite:execute-non-query
            *db-conn*
            "UPDATE client SET prenom = ?, nom = ?, telephone = ?, email = ? WHERE clientID = ?"
            prenom nom telephone email cid)
           (setf *current-user* (find-client-by-id cid))
           (hunchentoot:redirect "/mon-compte"))))))

;; -------------------------------
;; Changer mot de passe
;; -------------------------------

(easy-routes:defroute change-password-get ("/mon-compte/mot-de-passe" :method :get) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (djula:render-template* +change-password.html+ nil
        :current-user *current-user*
        :user-content *user-content*)))

(easy-routes:defroute change-password-post ("/mon-compte/mot-de-passe" :method :post) ()
  (if (null *current-user*)
      (hunchentoot:redirect "/connexion")
      (let* ((cid         (getf *current-user* :|clientID|))
             (params      (hunchentoot:post-parameters*))
             (old-pass    (cdr (assoc "old_password" params :test #'string=)))
             (new-pass    (cdr (assoc "new_password" params :test #'string=)))
             (confirm     (cdr (assoc "confirm_password" params :test #'string=)))
             (client      (find-user-by-email (getf *current-user* :|email|))))
        (cond
          ((not (string= (getf client :|mdp|) old-pass))
           (djula:render-template* +change-password.html+ nil
             :error "Ancien mot de passe incorrect."
             :current-user *current-user*
             :user-content *user-content*))
          ((not (string= new-pass confirm))
           (djula:render-template* +change-password.html+ nil
             :error "Les nouveaux mots de passe ne correspondent pas."
             :current-user *current-user*
             :user-content *user-content*))
          ((str:blankp new-pass)
           (djula:render-template* +change-password.html+ nil
             :error "Le nouveau mot de passe ne peut pas être vide."
             :current-user *current-user*
             :user-content *user-content*))
          (t
           (sqlite:execute-non-query
            *db-conn*
            "UPDATE client SET mdp = ? WHERE clientID = ?"
            new-pass cid)
           (hunchentoot:redirect "/mon-compte"))))))

;;
;; ────────────────────────────────────────────────────────────────────────────
;; Start / Stop
;; ────────────────────────────────────────────────────────────────────────────
;;

(defun get-port (port)
  (or port
      (ignore-errors (parse-integer (uiop:getenv "AB_PORT")))
      *port*))

(defun get-sentry-dsn ()
  (unless *dev-mode*
    (or (uiop:getenv "SENTRY_DSN")
        (when (uiop:file-exists-p *sentry-dsn-file*)
          (str:trim (str:from-file (uiop:native-namestring *sentry-dsn-file*)))))))


(defun start-server (&key port)
  (let ((port (get-port port)))
    (uiop:format! t "~&Starting the web server on port ~a" port)
    (force-output)
    (setf *server* (make-instance 'abstock-acceptor :port port))
    (setf *port* port)
    (hunchentoot:start *server*)
    (serve-static-assets)))

(defun restart-server (&key port)
  (hunchentoot:stop *server*)
  (start-server :port (get-port port)))

(defun start (&key port (load-init t) (load-db t) (post-init t))
  "If `load-db' is non t, do not load the books DB, but try to load saved cards on disk."
  (uiop:format! t "ABStock v~a~&" *version*)

  ;; --- Open SQLite early ---
  (unless *db-conn*
    (uiop:format! t "~&[db] Opening SQLite… (~a)~&" *db-path*)
    (db-init)
    (uiop:format! t "[db] OK.~&"))

  ;; Sentry
  (unless *dev-mode*
    (handler-case
        (let ((dsn (get-sentry-dsn)))
          (if (str:non-blank-string-p dsn)
              (progn
                (sentry-client:initialize-sentry-client dsn
                  :client-class 'sentry-client:async-sentry-client)
                (uiop:format! t "~&Sentry client initialized.~&"))
              (uiop:format! t "~&Sentry was not initialized.~&")))
      (error (c)
        (uiop:format! *error-output* "~&*** Starting Sentry client failed: ~a***~&" c))))

  ;; Init file
  (if load-init
      (progn
        (uiop:format! t "Loading init file...~&")
        (when (fboundp 'load-init) (load-init)))
      (uiop:format! t "Skipping init file.~&"))

  ;; Currency & cards bootstrap
  (uiop:format! t "db app name? ~a" *db-app-name*)
  (uiop:format! t "~&Getting the DEFAULT CURRENCY: ~a~&"
                (abstock/currencies::find-default-currency *db-app-name* :db-name *db-name*))
  (uiop:format! t "Loading data from cards.txt~&")
  (setf *cards* (normalise-cards (abstock/loaders:load-txt-data)))
  (uiop:format! t "Reloading saved cards, before reading the DB…")
  (when (fboundp 'reload-cards) (reload-cards))
  (uiop:format! t "~&Done.~&")

  (when (uiop:file-exists-p "selection.csv")
    (uiop:format! t "Loading cards selection…")
    (setf *selection* (read-selection))
    (uiop:format! t "~&Done.~&"))

  ;; Templates
  (log:info "current theme is " *theme*)
  (load-theme-templates)
  (load-templates)

  ;; Server
  (start-server :port port)
  (uiop:format! t "~&~a~&" (cl-ansi-text:green "✔ Ready. You can access the application!"))

  ;; Swank (optional)
  (handler-case
      (if *start-swank-server*
          (let ((swank-port (- *port* 5000)))
            (format t "~&Starting a Swank server on port ~a…~&" swank-port)
            (ql:quickload "swank")
            (swank-loader:init :load-contribs t)
            (swank:create-server :port swank-port :dont-close t))
          (format t "~&Let's not start a Swank server, alright.~&"))
    (error (c)
      (format t "~&Could not start Swank server: ~a~&" c)))

  ;; Admin URL
  (get-admin-url)
  (format-box t (format nil "Your admin URL is: ~a" *admin-url*))

  ;; External books DB (original app)
  (if load-db
      (progn
        (unless *connection* (setf *connection* (connect)))
        (uiop:format! t "~&Reading the DB...")
        (get-all-cards)
        (uiop:format! t "~&Reading all shelves...")
        (get-all-shelves)
        (uiop:format! t "~&Done. ~a cards found." (length *cards*))
        (when (fboundp 'schedule-db-reload) (schedule-db-reload))
        (uiop:format! t "~&Scheduled a DB reload every night.~&")
        (save))
      (uiop:format! t "~&Skipped loading the DB.~&"))

  ;; Post-init
  (if post-init
      (when (fboundp 'load-post-init) (load-post-init))
      (uiop:format! t "Skipping post-init file.~&")))

(defun stop ()
  (when *db-conn*
    (ignore-errors (sqlite:disconnect *db-conn*))
    (setf *db-conn* nil))
  (hunchentoot:stop *server*))

(defun toggle-dev-mode ()
  (format t "*dev-mode*: ~a (you won't see as much books)~&" (setf *dev-mode* (not *dev-mode*)))
  (format t "hunchentoot catch errors: ~a~&"
          (setf hunchentoot:*catch-errors-p* (not hunchentoot:*catch-errors-p*))))
