;;;; abstock/shipping.lisp — Colissimo (statique, France Métro)
;;;; Source tarifs HT : PDF "Mes Conditions Tarifaires – Compte 316106" (22/08/2025) :contentReference[oaicite:0]{index=0}

(defpackage :abstock/shipping
  (:use :cl)
  (:export
   *shipping-backend*
   *store-postcode* *store-city* *store-country*
   rates-for-address find-rate-by-id estimate-total-weight))

(in-package :abstock/shipping)

;; Backend choisi
(defparameter *shipping-backend* :colissimo-static)

;; Adresse d’expédition (librairie)
(defparameter *store-postcode* "75001")
(defparameter *store-city*     "Paris")
(defparameter *store-country*  "FR")

;; Poids par article : lit :|weight| ou :weight, défaut 0.35 kg (livre broché)
(defun weight-of-card (card)
  (or (getf card :|weight|) (getf card :weight) 0.35d0))

(defun estimate-total-weight (cards)
  (reduce #'+ (mapcar #'weight-of-card cards) :initial-value 0d0))

;; --- Colissimo France Métropolitaine (HT) ---
;; colonnes : (limite-kg  :home-ss  :home-sig  :relay) — valeurs en € HT
(defparameter *colissimo-fr-rates*
  '((0.25 :home-ss 6.72  :home-sig 7.77  :relay 5.11)
    (0.50 :home-ss 7.57  :home-sig 8.62  :relay 5.95)
    (0.75 :home-ss 8.45  :home-sig 9.50  :relay 6.85)
    (1.00 :home-ss 9.17  :home-sig 10.22 :relay 7.57)
    (2.00 :home-ss 10.29 :home-sig 11.34 :relay 8.68)
    (3.00 :home-ss 11.29 :home-sig 12.34 :relay 9.68)
    (4.00 :home-ss 12.32 :home-sig 13.37 :relay 10.70)
    (5.00 :home-ss 13.30 :home-sig 14.35 :relay 11.69)
    (6.00 :home-ss 13.92 :home-sig 14.97 :relay 12.32)
    (7.00 :home-ss 14.89 :home-sig 15.94 :relay 13.28)
    (8.00 :home-ss 15.86 :home-sig 16.91 :relay 14.26)
    (9.00 :home-ss 16.87 :home-sig 17.92 :relay 15.26)
    (10.0 :home-ss 17.85 :home-sig 18.90 :relay 16.23)
    (11.0 :home-ss 18.46 :home-sig 19.51 :relay 16.86)
    (12.0 :home-ss 19.42 :home-sig 20.47 :relay 17.82)
    (13.0 :home-ss 20.37 :home-sig 21.42 :relay 18.77)
    (14.0 :home-ss 21.37 :home-sig 22.42 :relay 19.76)
    (15.0 :home-ss 22.33 :home-sig 23.38 :relay 20.71)
    (16.0 :home-ss 23.28 :home-sig 24.33 :relay 21.67)
    (17.0 :home-ss 24.24 :home-sig 25.29 :relay 22.63)
    (18.0 :home-ss 25.21 :home-sig 26.26 :relay 23.59)
    (19.0 :home-ss 26.18 :home-sig 27.23 :relay 24.57)
    (20.0 :home-ss 27.13 :home-sig 28.18 :relay 25.51)
    (21.0 :home-ss 27.83 :home-sig 28.88 :relay 26.21)
    (22.0 :home-ss 28.77 :home-sig 29.82 :relay 27.16)
    (23.0 :home-ss 29.74 :home-sig 30.79 :relay 28.13)
    (24.0 :home-ss 30.70 :home-sig 31.75 :relay 29.08)
    (25.0 :home-ss 31.62 :home-sig 32.67 :relay 30.02)
    (26.0 :home-ss 32.60 :home-sig 33.65 :relay 30.98)
    (27.0 :home-ss 33.53 :home-sig 34.58 :relay 31.93)
    (28.0 :home-ss 34.49 :home-sig 35.54 :relay 32.89)
    (29.0 :home-ss 35.48 :home-sig 36.53 :relay 33.86)
    (30.0 :home-ss 36.39 :home-sig 37.44 :relay 34.79)))

(defparameter *colissimo-fr-vat* 0.20) ;; TVA 20%

(defun %eurht->cents-ttc (eur-ht)
  (round (* (+ eur-ht (* eur-ht *colissimo-fr-vat*)) 100)))

(defun %bracket-for-kg (kg table)
  "Retourne la ligne dont la limite >= kg ; sinon la dernière."
  (or (find-if (lambda (row) (<= kg (first row))) table)
      (car (last table))))

(defun %fr-offers-for-kg (kg)
  (let* ((row (%bracket-for-kg kg *colissimo-fr-rates*))
         (limit (first row))
         (home-ss (getf (rest row) :home-ss))
         (home-sig (getf (rest row) :home-sig))
         (relay   (getf (rest row) :relay)))
    (declare (ignore limit))
    (remove nil
            (list
             (and relay
                  (list :id "colissimo_relay_fr"
                        :provider "Colissimo"
                        :label "Point Relais"
                        :eta "48–72h"
                        :price (%eurht->cents-ttc relay)))
             (and home-ss
                  (list :id "colissimo_home_ss_fr"
                        :provider "Colissimo"
                        :label "Domicile (sans signature)"
                        :eta "48–72h"
                        :price (%eurht->cents-ttc home-ss)))
             (and home-sig
                  (list :id "colissimo_home_sig_fr"
                        :provider "Colissimo"
                        :label "Domicile (avec signature)"
                        :eta "48–72h"
                        :price (%eurht->cents-ttc home-sig)))))))

(defun %country-fr? (pays)
  (let ((u (string-upcase (princ-to-string (or pays "FR")))))
    (or (string= u "FR") (string= u "FRANCE"))))

;; API publique — retourne une liste d'offres (:id :provider :label :eta :price en centimes TTC)
(defun rates-for-address (address cards)
  (destructuring-bind (&key pays &allow-other-keys) address
    (let ((kg (max 0.01d0 (estimate-total-weight cards))))
      (cond
        ((%country-fr? pays) (%fr-offers-for-kg kg))
        (t '()))))) ;; international/OM non géré ici

(defun find-rate-by-id (rates rid)
  (find rid rates :key (lambda (r) (getf r :id)) :test #'string=))
