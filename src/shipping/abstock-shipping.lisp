(in-package #:cl-user)

(defpackage #:abstock/shipping
  (:use #:cl)
  (:export
   #:rates-for-address
   #:find-rate-by-id
   #:*store-country*
   #:*store-city*
   #:*store-postcode*))

(in-package #:abstock/shipping)

;; Paramètres magasin par défaut (utilisés si on ne passe rien à rates-for-address)
(defparameter *store-country* "FR")
(defparameter *store-city*    "Paris")
(defparameter *store-postcode* "75000")

;; Grille de secours : toujours au moins un mode dispo
(defun %fallback-grid ()
  (list
   (list :id "sur_place"
         :label "Retrait sur place"
         :price 0
         :eta "immédiat"
         :provider "Boutique")
   (list :id "colissimo"
         :label "Colissimo (forfait)"
         :price 5
         :eta "2-3 jours"
         :provider "La Poste")))

;; Trouver un tarif par son :id dans une grille
(defun find-rate-by-id (id &optional (grid (%fallback-grid)))
  (find id grid :key (lambda (r) (getf r :id)) :test #'string=))

;; API publique appelée par le reste de l'appli
(defun rates-for-address (&key address zip city country weight)
  (declare (ignore address weight))
  (let* ((country (or country *store-country*))
         (city    (or city    *store-city*))
         (zip     (or zip     *store-postcode*)))
    (declare (ignore country city zip))
    (handler-case
        ;; Ici on retournerait la vraie grille calculée ; on renvoie le fallback
        (%fallback-grid)
      (error (e)
        (declare (ignore e))
        (%fallback-grid)))))
