;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Newcastle University

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

(ns tawny.read-test
  (:refer-clojure :exclude [read])
  (:use [clojure.test])
  (:require [tawny.read :as r]
            [tawny.owl :as o])
  (:import (org.semanticweb.owlapi.model IRI OWLNamedObject OWLOntologyID)
           (org.semanticweb.owlapi.util SimpleIRIMapper))

  )

(defn get-go-ontology []
  (tawny.owl/remove-ontology-maybe
   (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
  (.loadOntologyFromOntologyDocument
   tawny.owl/owl-ontology-manager
   (IRI/create (clojure.java.io/resource "go-snippet.owl"))))


(deftest read
  (is
   (do
     (try
       ;; at some point this needs support in tawny.
       (.addIRIMapper
        tawny.owl/owl-ontology-manager
        (SimpleIRIMapper.
         (IRI/create "http://www.w3.org/TR/2003/PR-owl-guide-20031209/food")
         (IRI/create (clojure.java.io/resource "food.rdf"))))

       (r/read :location
               (IRI/create (clojure.java.io/resource "wine.rdf"))
               :iri "http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine"
               :prefix "wine:"
               )
       ;; we shouldn't be using this again, but clean up anyway.
       (finally
         (doseq
             [o
              (.getOntologies tawny.owl/owl-ontology-manager)]
           (tawny.owl/remove-ontology-maybe (.getOntologyID o)))
         (.clearIRIMappers tawny.owl/owl-ontology-manager))))))


(deftest read-with-mapper
  (is
   (try
     (r/read :location
             (IRI/create (clojure.java.io/resource "wine.rdf"))
             :iri "http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine"
             :prefix "wine:"
             :mapper
             (r/resource-iri-mapper
              {"http://www.w3.org/TR/2003/PR-owl-guide-20031209/food",
               "food.rdf"}))
     (finally
         (doseq
             [o
              (.getOntologies tawny.owl/owl-ontology-manager)]
           (tawny.owl/remove-ontology-maybe (.getOntologyID o)))))))

(deftest go-snippet-as-resource
  (is (clojure.java.io/resource "go-snippet.owl")))

(deftest go-snippet-filter
  (is
   (every?
    (comp not nil?)
    (filter (partial #'r/default-filter (get-go-ontology))
            (.getSignature (get-go-ontology)))))

  (is
   (every?
    (comp not nil?)
    (filter (partial r/iri-starts-with-filter
                     "http://purl.obolibrary.org/obo/go")
            (.getSignature (get-go-ontology))))))



;; need to test out label-transform
;; need to test out stop-characters-transform
(deftest go-snippet-transform
  (is
   (every?
    (comp not nil?)
    (let [fil
          (doall
           (map #'r/default-transform
                ;; this transform works on the anntotations which start with
                ;; "obo/go" rather than "obo/GO"
                (filter (partial
                         r/iri-starts-with-filter "http://purl.obolibrary.org/obo/go")
                        (.getSignature (get-go-ontology)))))]
      ;; let form allows me to add println -- not totally stupid
      ;;(println "first fil:" fil)
      fil)))


  (is
   (do
     (let [fil
           (doall
            (map (partial r/label-transform (get-go-ontology))
                 ;; the label transform should work on "GO" terms should return
                 ;; one annotation and one nil which it is superclass. We have
                 ;; chopped the annotation out of the snippet.
                 (filter (partial r/iri-starts-with-filter
                                  "http://purl.obolibrary.org/obo/GO")
                         (.getSignature (get-go-ontology)))))]
       ;;(println "This Fil:" fil)
       fil
       ))
   )

  (is
   (thrown?
    IllegalArgumentException
    (let [fil
          (doall
           (map (partial r/exception-nil-label-transform (get-go-ontology))
                      ;; this should throw an exception because we have one class without
                      ;; annotation
                      (filter (partial r/iri-starts-with-filter
                                       "http://purl.obolibrary.org/obo/GO")
                              (.getSignature (get-go-ontology)))))]
      ;;(println  "Fil 1:" fil)
      fil
     )
    ))


  )

(deftest go-snippet-read
  (is (r/read :location
              (IRI/create (clojure.java.io/resource "go-snippet.owl"))
              :iri "http://purl.obolibrary.org/obo/go.owl"
              :prefix "go:")))


(deftest stop-characters-transform
  (is (= "bob" (r/stop-characters-transform "bob")))
  (is (= "bob_" (r/stop-characters-transform "bob(")))
  (is (= "bob_bob" (r/stop-characters-transform "bob bob")))
  (is (= "_9bob" (r/stop-characters-transform "9bob")))
  (is (= "_9_bob" (r/stop-characters-transform "9 bob")))
  (is (= "_9_bob" (r/stop-characters-transform " 9 bob"))))
