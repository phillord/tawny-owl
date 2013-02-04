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



(ns tawny.repl-test
  (:use [clojure.test])
  (:require [tawny.repl :as re]
            [tawny.owl :as o])
  (:import (org.semanticweb.owlapi.model 
            OWLOntologyID IRI)))


(o/defontology testontology
  :iri "http://iri/"
  :prefix "iri:"
  )

(o/declare-classes testsuperclass)

(o/defclass testclass
  :subclass testsuperclass
  :label "This is label in English"
  :annotation (o/label "Questi e una targhetta in Italiano" "it")
  :comment "This is a comment")


(deftest fetch-doc
  (is (re/fetch-doc testclass testontology)))

(defn get-go-ontology []
  (tawny.owl/remove-ontology-maybe 
   (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
  (.loadOntologyFromOntologyDocument
   tawny.owl/owl-ontology-manager 
   (IRI/create (clojure.java.io/resource "go-snippet.owl"))))



