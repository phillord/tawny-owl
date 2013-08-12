;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, Phillip Lord, Newcastle University
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

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/" :prefix "iri"))))


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



