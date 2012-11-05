(ns owl.test.reasoner
  (:refer-clojure :exclude [some only comment])
  (:require [owl.owl :as o]
            [owl.reasoner :as r])
  [:use clojure.test])


(defn createtestontology[]
  (o/defontology testontology
    :file "test.omn"
    :iri "http://iri/"
    :prefix "iri:"))

(defn createandsavefixture[test]
  (createtestontology)
  (test)
  (o/save-ontology))

(use-fixtures :each createandsavefixture)

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


(deftest consistent? []
  ;; empty only is consistent
  (is
   (every?
    identity
    (far #(r/consistent?))))

  ;; with individual
  (is
   (every?
    identity
    (do
      (ontology-abc-indc)
      (far #(r/consistent?)))))

  ;; simple ontology without ind
  (is
   (every?
    identity
    (do
      (ontology-abc)
      (far #(r/consistent?)))))

  ;; without ind -- should be incoherent
  (is
   (every?
    complement
    (far #(do
            (ontology-abc)
            (o/disjointclasses "a" "b")
            (r/consistent?))))))

  
  ;; now ontology should be inconsistent also
  (is
   (every?
    complement
    (do
      (ontology-abc-indc)
      (o/disjointclasses "a" "b")
      (far #(r/consistent?)))))

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