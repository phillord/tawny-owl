;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns tawny.reasoner-test
  (:refer-clojure :exclude [some only comment])
  (:require [tawny.owl :as o]
            [tawny.reasoner :as r])
  [:use clojure.test])

(def to nil)
(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology
      :iri "http://iri/"
      :prefix "iri:"))))

(defn createandsavefixture[test]
  (binding
      [r/*reasoner-progress-monitor*
       (atom r/reasoner-progress-monitor-silent)]
    (createtestontology)
    (test)
    ;;(o/save-ontology to "test-reasoner.omn")
    ))



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
    (nil? to))))

(defn ontology-abc []
  (o/owl-class to "a")
  (o/owl-class to "b")
  (o/owl-class to "c" :super "a" "b"))

(defn ontology-abc-indc []
  (ontology-abc)
  (o/individual to "indC" :type "c"))


(defn ontology-abc-reasoning []
  ;; simple ontology -- c should be reasoned to be a subclass of a.
  (o/owl-class to "a"
              :equivalent 
              (o/object-some to "p" "b"))
  (o/owl-class to "b")
  (o/owl-class to "c"
              :super
              (o/object-some to "p" "b")))


(defn far-reasoner [func reasonerlist]
  ;; lazy sequences are crazy
  (doall
   (map
    (fn [x]
      (r/reasoner-factory x)
      (func))
    reasonerlist)))

;; for all reasoners
(defn far [func]
  (far-reasoner func
                '(:elk :hermit :jfact)))

;; for all dl reasoners
(defn fadlr [func]
  (far-reasoner func
                '(:hermit :jfact)))
;; (map
;;  (fn [x]
;;    (r/reasoner-factory x)
;;    (r/consistent?))
;;  '(:elk :hermit))


(deftest no-reasoner-set
  (is
   (thrown?
    IllegalStateException
    (dosync
     (ref-set (var-get #'r/vreasoner-factory) nil)
     (r/reasoner to)))))

(deftest empty-consistent?
  ;; empty only is consistent
  (is
   (every?
    identity
    (far #(r/consistent? to)))))


(deftest ind-consistent?
  ;; with individual
  (is
   (every?
    identity
    (do
      (ontology-abc-indc)
      (far #(r/consistent? to))))))


(deftest simple-consistent []
  ;; simple ontology without ind
  (is
   (every?
    identity
    (do
      (ontology-abc)
      (far #(r/consistent? to))))))

(deftest disjoint-consistent []
  ;; without ind -- should be incoherent
  (is
   (every?
    complement
    (do
      (ontology-abc)
      (o/as-disjoint to "a" "b")
      (far #(r/consistent? to))))))

  
(deftest disjoint-and-individual []
  ;; now ontology should be inconsistent also
  (is
   (every?
    complement
    (do
      (ontology-abc-indc)
      (o/as-disjoint to "a" "b")
      (far #(r/consistent? to))))))

(deftest unsatisfiable []
  (is
   (every?
    #(= 0 (count %))
    (do (ontology-abc)
        (far #(r/unsatisfiable to)))))
  (is
   (every?
    #(= 1 (count %))
    (do
      (ontology-abc)
      (o/as-disjoint to "a" "b")
      (far #(r/unsatisfiable to))))))

;; had lots of problems getting this working so lets try with a single reasoner
(deftest single-coherent
  (is
   (do
     (r/reasoner-factory :hermit)
     (ontology-abc)
     (r/coherent? to))))


(deftest coherent
  (is
   (every?
    identity
    (do
      (ontology-abc)
      (far #(do 
              (r/coherent? to)))))))


(deftest incoherent
  (is
   (every?
    not
    (do
      (ontology-abc)
      (o/as-disjoint to "a" "b")
      (far #(r/coherent? to))))))


(deftest isuperclass?
  (is
   (every? 
    identity
    (do 
      (ontology-abc-reasoning)
      (far #(r/isuperclass? to
             (o/owl-class to "c") 
             (o/owl-class to "a")))))))

(deftest isubclass?
  (is
   (every?
    identity
    (do
      (ontology-abc-reasoning)
      (far #(r/isubclass? to
             (o/owl-class to "a")
             (o/owl-class to "c"))))
    )))




(deftest with-probe-axioms
  ;; add a disjoint see whether it breaks
  (is
   (every?
    not
    (do
      (ontology-abc)
      (o/with-probe-axioms to
        [a (o/as-disjoint to "a" "b")]
        (doall (far #(r/coherent? to)))))))

  ;; add a disjoint test whether it breaks after
  (is 
   (every?
    identity
    (do 
      (ontology-abc)
      (o/with-probe-axioms to
        [a (o/as-disjoint to "a" "b")])
      (doall (far #(r/coherent? to)))))))


;; Nice idea for a test, but in a non-headless environment, it starts
;; a GUI thread, and then nothing exists.
;; (deftest reasoner-gui-maybe
;;   (is
;;    (instance? org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor
;;               (r/reasoner-progress-monitor-gui-maybe)) ()))
