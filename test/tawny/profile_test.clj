;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University
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

(ns tawny.profile-test
  (:use [clojure.test])
  (:require [tawny.profile :as p]
            [tawny.owl :as o]))

(def to nil)

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://example.com"))))

(defn createandsavefixture[test]
  (let [exp
        #(throw (Exception. "default ontology used"))
        trace
        #(tawny.debug/tracing-println "default ontology used")
        ]
    (when true
      (tawny.util/add-hook
       o/default-ontology-hook exp
       ))
    (when false
      (tawny.util/add-hook
       o/default-ontology-hook trace))
    (createtestontology)
    (test)
    (tawny.util/remove-hook o/default-ontology-hook exp)
    (tawny.util/remove-hook o/default-ontology-hook trace)))

(use-fixtures :each createandsavefixture)

(deftest inprofile-owl2dl?
  (is
   (p/inprofile? to p/profile-owl2dl)))

(deftest inprofile-owl2
  (is
   (p/inprofile? to p/profile-owl2)))

(deftest inprofile-owl2el
  (is
   (p/inprofile? to p/profile-owl2el)))
