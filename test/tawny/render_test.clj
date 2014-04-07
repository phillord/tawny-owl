;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2014, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.
(ns tawny.render-test
  (:use [clojure.test])
  (:require [tawny.render :as r]
            [tawny.owl :as o]))

(def to nil)

(defn createtestontology[test]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/"
                 :noname true
                 :prefix "iri")))
  (test))

(use-fixtures :each createtestontology)

(deftest datatype
  (is (= :XSD_INTEGER
         (r/form (#'o/ensure-datatype to :XSD_INTEGER)))))

(defn lit-f
  ([val]
     (r/form (o/literal val)))
  ([val lang]
     (r/form (o/literal val :lang lang))))

(deftest literal
  (is
   (= '(literal "10" :type :XSD_INTEGER)
      (lit-f 10))))
;;   (is
;;    (= [10.0]
;;       (lit-f 10.0)))
;;   (is
;;    (= [true]
;;       (lit-f true)))
;;   (is
;;    (= ["bob"]
;;       (lit-f "bob")))
;;   (is
;;    (= ["bob" "en"]
;;       (lit-f "bob" "en"))))

(defn data-ontology []
  (o/datatype-property to "rD"))

(deftest datasome-datatype
  (is
   (=
    '(owl-some (iri "http://iri/#rD") :XSD_INTEGER)

    (do (data-ontology)
        (r/form
         (first (o/owl-some to "rD" :XSD_INTEGER)))))))

(deftest datasome-range
  (is
   (= '(owl-some (iri "http://iri/#rD") (span < 1))
                (first
                 (r/form
                  (o/owl-some to "rD" (o/span < 1)))))))


(deftest individual-fact-1
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2")))
      (r/as-form
       (o/individual to "I"
                     :fact (o/fact to (o/object-property to "r")
                                   (o/individual to "I2")))))))

(deftest individual-fact-2
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact-not (iri "http://iri/#r")
                             (iri "http://iri/#I2")))
      (r/as-form
       (o/individual to "I"
                     :fact (o/fact-not to (o/object-property to "r")
                                   (o/individual to "I2")))))))


(deftest individual-3
  (is
   (=
    `(individual (iri "http://iri/#I")
                :fact
                (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2"))
                (fact-not (iri "http://iri/#r")
                          (iri "http://iri/#I2"))))
   (r/as-form
    (o/individual to "I"
                  :fact
                     (o/fact to (o/object-property to "r")
                             (o/individual to "I2"))
                     (o/fact-not to (o/object-property to "r")
                                 (o/individual to "I2"))))))


(deftest individual-data
  (is
   (=
    '(individual
      (iri "http://iri/#I")
      :fact (fact (iri "http://iri/#d")
                  (literal "10" :type :XSD_INTEGER)))
    (r/as-form
     (o/individual to "I"
                   :fact
                   (o/fact to (o/datatype-property to "d")
                           10))))))


(deftest individual-data-2
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2"))
                   (fact (iri "http://iri/#d")
                         (literal "10" :type :XSD_INTEGER)))
      (r/as-form
       (o/individual to "I"
                     :fact
                     (o/fact to (o/datatype-property to "d")
                             10)
                     (o/fact to (o/object-property to "r")
                             (o/individual to "I2")))))))

(deftest oproperty-super-test
  (is
   (= '(object-property
        (iri "http://iri/#r")
        :super
        (iri "http://iri/#s"))
      (r/as-form
       (o/object-property to "r"
                          :super
                          (o/iri-for-name to "s"))))))

(deftest dproperty-super-test
  (is
   (= '(datatype-property
        (iri "http://iri/#g")
        :super
        (iri "http://iri/#h"))
      (r/as-form
       (o/datatype-property to "g"
                          :super
                          (o/iri-for-name to "h"))))))
