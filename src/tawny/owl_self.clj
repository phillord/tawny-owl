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

(in-ns 'tawny.owl)

;; Self-Annotation capabilities for tawny.
;; To avoid boot problems, will replicate some code here.

;; this is much the same as the external annotation but a requires an
;; annotation property object
(defn- tawny-annotation
  [property ^String literal]
  (.getOWLAnnotation
   (owl-data-factory) property
   (.getOWLLiteral (owl-data-factory) literal "en")))

(defn- tawny-annotation-property
  [iri]
  (.getOWLAnnotationProperty (owl-data-factory) iri))

(def ^{:private true} tawny-base-url
  "http://www.purl.org/ontolink/tawny")

(def ^{:private true} tawny-iri
  (iri tawny-base-url))

(def
  ^{:private true}
  tawny-name-property
  (tawny-annotation-property
   (iri (str tawny-base-url "#name"))))

(defn- tawny-name [literal]
  (tawny-annotation tawny-name-property literal))

(defonce ^{:private true}
  vtawny-ontology (ref nil))

(defn- tawny-ontology
  "Lazy load and return the tawny ontology."
  []
  (if-not @vtawny-ontology
    (dosync
     (ref-set
      vtawny-ontology
      (.loadOntologyFromOntologyDocument
       (tawny.owl/owl-ontology-manager)
       (iri (clojure.java.io/resource "tawny.owl")))))
    @vtawny-ontology))
