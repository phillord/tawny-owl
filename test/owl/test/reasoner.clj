;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns owl.test.reasoner
  (:refer-clojure :exclude [some only comment])
  (:require [owl.owl :as o]
            [owl.reasoner :as r])
  [:use clojure.test])


(defn createtestontology[]
  (o/ontology 
   :iri "http://iri/"
   :prefix "iri:"))

(defn createandsavefixture[test]
  (binding
      [r/*reasoner-progress-monitor*
       r/reasoner-progress-monitor-text]
    (o/with-ontology
      (createtestontology)
      (test)
      (o/save-ontology "test-reasoner.omn"))))

;; this isn't working and I really don't know why
;; it seems to work on lein test but kills all tests
;; when run in repl with clojure-test mode. 

;; works fine in fixture above so leave it there
(defn reasoner-gui-fixture [tests]
  (binding [r/*reasoner-progress-monitor*
            r/reasoner-progress-monitor-text]
    (tests)))



(use-fixtures
 ;;:once reasoner-gui-fixture
 :each createandsavefixture)


(defn with-ontology []
  (is
   (not
    (nil? (o/get-current-ontology)))))

(defn ontology-abc []
  (o/owlclass "a")
  (o/owlclass "b")
  (o/owlclass "c" :subclass "a" "b"))

(defn ontology-abc-indc []
  (ontology-abc)
  (o/individual "indC" :types "c"))

(defn far-reasoner [func reasonerlist]
  (map
   (fn [x]
     (r/reasoner-factory x)
     (func))
   reasonerlist))

;; for all reasoners
(defn far [func]
  (far-reasoner func
                '(:elk :hermit)))

;; for all dl reasoners
(defn fadlr [func]
  (far-reasoner func)
  '(:hermit))
;; (map
;;  (fn [x]
;;    (r/reasoner-factory x)
;;    (r/consistent?))
;;  '(:elk :hermit))


(deftest empty-consistent? []
  ;; empty only is consistent
  (is
   (do (println "hello")
       (every?
        identity
        (far #(r/consistent?))))))


(deftest ind-consistent? []
  ;; with individual
  (is
   (every?
    identity
    (do
      (ontology-abc-indc)
      (far #(r/consistent?))))))


(deftest simple-consistent []
  ;; simple ontology without ind
  (is
   (every?
    identity
    (do
      (ontology-abc)
      (far #(r/consistent?))))))

(deftest disjoint-consistent []
  ;; without ind -- should be incoherent
  (is
   (every?
    complement
    (do
      (ontology-abc)
      (o/disjointclasses "a" "b")
      (far #(r/consistent?))))))

  
(deftest disjoint-and-individual []
  ;; now ontology should be inconsistent also
  (is
   (every?
    complement
    (do
      (ontology-abc-indc)
      (o/disjointclasses "a" "b")
      (far #(r/consistent?))))))

(deftest unsatisfiable []

  (is
   (every?
    #(= 0 (count %))
    (do (ontology-abc)
        (far #(r/unsatisfiable)))))

  (is
   (every?
    #(= 1 (count %))
    (do
      (ontology-abc)
      (o/disjointclasses "a" "b")
      (far #(r/unsatisfiable))))))


(deftest coherent []
  (is
   (every?
    identity
    (do
      (ontology-abc)
      (far #(r/coherent?))))))