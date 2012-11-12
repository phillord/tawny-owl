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


(ns owl.test.owl
  (:refer-clojure :exclude [some only comment])
  (:require [owl.owl :as o])
  [:use clojure.test])

(defn createtestontology[]
  (o/defontology testontology
    :file "test.omn"
    :iri "http://iri/"
    :prefix "iri:"))

(defn createandsavefixture[test]
  (createtestontology)
  (test)
  (o/save-ontology)
  )

(use-fixtures :each createandsavefixture)

(deftest ontology
  (is true))

(deftest defontology
  (is (not (nil? testontology)))
  (is (= 0 (.getAxiomCount (#'o/get-current-jontology)))))

(deftest get-current-ontology
  (is (not (nil? (o/get-current-ontology))))
  (is (= testontology (o/get-current-ontology))))

(deftest set-current-ontology
  (is (do
        (let [current-ont (o/get-current-ontology)
              ;; redefines current ontology to new ont
              newont
              (o/defontology testontology2
                :file "test.omn" :iri "http://iri/" :prefix "iri2:")]
          (o/set-current-ontology current-ont)
          ;; the actual test
          (= current-ont (o/get-current-ontology))))))
        
(deftest get-current-jontology
  (is (not (nil? (#'o/get-current-jontology)))))

(deftest get-current-iri
  (is (= "http://iri/" (.toString (#'o/get-current-iri)))))

(deftest get-current-file []
  (is (= "test.omn" (o/get-current-file))))

(deftest get-current-manager []
  (is (not (nil? (#'o/get-current-manager)))))

(deftest get-current-prefix []
  (is (= "iri:" (o/get-current-prefix))))

(deftest save-ontology []
  (is (do (o/save-ontology)
          true)))

(deftest iriforname []
  (is (= (.toString (#'o/iriforname "test"))
         "http://iri/#test")))

(deftest get-create-object-property []
  (is (instance? org.semanticweb.owlapi.model.OWLObjectProperty
                 (#'o/get-create-object-property "hello"))))


(deftest ensure-object-property []
  (is 
   ;; check whether it makes an object out of a string
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (#'o/ensure-object-property "hello")))
  (is
   ;; check whether it makes keeps an object as an object
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (#'o/ensure-object-property
               (#'o/get-create-object-property "hello")))))

(deftest defoproperty
  (is
   (instance? owl.owl.AxiomedEntity
              (o/defoproperty a))
   
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (:entity (o/defoproperty a)))))


(deftest get-create-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (#'o/get-create-class "hello")))
  (is  (=  (.hashCode (#'o/get-create-class "hello"))
           (.hashCode (#'o/get-create-class "hello")))))


(deftest ensure-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class "hello")))
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class (#'o/get-create-class "hello")))))

(deftest create-subclass-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLSubClassOfAxiom
                 (#'o/create-subclass-axiom
                  (#'o/ensure-class "a") "b"))))

(deftest add-one-frame []
  (is (not
       (nil?
        (#'o/add-one-frame
         #'o/create-subclass-axiom
         (#'o/ensure-class "a") "b")))))

(deftest add-frame []
  (let [frame-call
        (#'o/add-frame
         #'o/create-subclass-axiom
         (#'o/ensure-class "a") '( "b" "c" "d"))]
    (is (not (nil? frame-call)))))



(deftest create-equiavlent-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
                 (#'o/create-equivalent-axiom
                  (#'o/ensure-class "a") "b"))))

(deftest add-equivalent []
  (let [equiv (#'o/add-equivalent
               (#'o/ensure-class "a")
               (list (#'o/ensure-class "b")))]
    (is (not (nil? equiv)))
))

(deftest create-class-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLDeclarationAxiom
                 (#'o/create-class-axiom (#'o/ensure-class "a") "b"))))

(deftest add-class []
  (is (not
       (nil?
        (o/add-class "a")))))

(deftest objectproperty []
  (is (instance? owl.owl.AxiomedEntity
                 (o/objectproperty "b")))

  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectProperty
       (:entity (o/objectproperty "b"))))
  )

(deftest owlsome []
  (is (not (nil? (o/owlsome (o/objectproperty "b") "a")))))

(deftest owlonly []
  (is (not (nil? (o/only (o/objectproperty "b") "a")))))

(deftest disjointclasses []
  (is
   (do (o/disjointclasses "a" "b" "c")))

  (is
   (do (o/disjointclasses
        (o/owlclass "a") (o/owlclass "b")))))

(deftest owlclass
  (is (= 1
         (do (o/owlclass "test")
             (.size (.getClassesInSignature
                     (#'o/get-current-jontology))))))
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (:entity (o/owlclass "test")))))


(deftest defclass
  (is (= 1
         (do (o/defclass a)
             (.size (.getClassesInSignature
                     (#'o/get-current-jontology))))))
  (is (not
       (nil?
        (do (o/defclass a)
            a)))))



(defn- test-class-with-hierarchy
  "Some test classes

Assumes that fixture has been run
"
  []
  
  (o/owlclass "a")
  (o/owlclass "b" :subclass "a")
  (o/owlclass "c" :subclass "b")

  (o/owlclass "d")
  (o/owlclass "e" :subclass "b" "d")
  )


(deftest superclass? []
  (is (not
       (nil?
        (do
          (test-class-with-hierarchy)
          (o/isuperclasses "c")))))
  (is (do
        (test-class-with-hierarchy)
        (o/superclass? "e" "a")))
  (is (not
       (do
         (test-class-with-hierarchy)
         (o/superclass? "c" "e")))))


(deftest subclass? []
  (is
   (do (test-class-with-hierarchy)
       (o/subclass? "a" "c")))
  (is
   (not
    (do (test-class-with-hierarchy)
        (o/subclass? "c" "e")))))


(deftest disjointclasses []
  (is (not (nil? (o/disjointclasses "a" "b" "c")))))

(deftest individual []
  (is (o/individual "ind"))
  (is (not (nil? (o/individual "indA" :types "a"))))
  (is (thrown? IllegalArgumentException
               (o/individual "indA" :nottypes "a"))))

(deftest defindividual []
  (is (do (o/defindividual testind)
          testind)))

(deftest remove-entity []
  (is
   (= 0
      (do
        (let [clazz (o/owlclass "a")]
          (o/remove-entity clazz)
          (.size (.getClassesInSignature
                  (#'o/get-current-jontology)))))))
  
  (is
   (= 0
      (do
        (let [prop (o/objectproperty "a")]
          (o/remove-entity prop)
          (.size (.getClassesInSignature
                  (#'o/get-current-jontology))))))))


;; TODO lots of macros are in serious need of a test


;; (subclass?
;;  (owlclass "test1")
;;  (owlclass "test"
;;            :subclass "test1"))