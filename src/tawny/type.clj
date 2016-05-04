;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, 2013, 2014, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See they
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns
    ^{:doc "Type based predicates for OWL."
      :author "Phillip Lord"}
    tawny.type
  (:import (org.semanticweb.owlapi.model
            IRI
            OWLAnnotationProperty
            OWLAnnotationValue
            OWLClass
            OWLClassExpression
            OWLDataProperty
            OWLDataPropertyExpression
            OWLDataRange
            OWLDatatype
            OWLIndividual
            OWLLiteral
            OWLNamedObject
            OWLObjectPropertyExpression
            OWLOntology)))

(defn iri?
  "Return true if `e` is an instance of `IRI`."
  [e]
  (instance? IRI e))

(defn ann-val?
  "Return true if `e` is an instance of `OWLAnnotationValue`."
  [e]
  (instance? OWLAnnotationValue e))

(defn ann-prop?
  "Return true if `e` is an instance of `OWLAnnotationProperty`."
  [e]
  (instance? OWLAnnotationProperty e))

(defn owl-class?
  "Return true if `e` is an instance of `OWLClass`."
  [e]
  (instance? OWLClass e))

(defn class-exp?
  "Return true if `e` is an instance of `OWLClassExpression`."
  [e]
  (instance? OWLClassExpression e))

(defn data-prop?
  "Return true if `e` is an instance of `OWLDataProperty`."
  [e]
  (instance? OWLDataProperty e))

(defn data-prop-exp?
  "Return true if `e` is an instance of `OWLDataPropertyExpression`."
  [e]
  (instance? OWLDataPropertyExpression e))

(defn data-range?
  "Return true if `e` is an instance of `OWLDataRange`."
  [e]
  (instance? OWLDataRange e))

(defn data-type?
  "Return true if `e` is an instance of `OWLDatatype`."
  [e]
  (instance? OWLDatatype e))

(defn individual?
  "Return true if `e` is an instance of `OWLIndividual`."
  [e]
  (instance? OWLIndividual e))

(defn literal?
  "Return true if `e` is an instance of `OWLLiteral`."
  [e]
  (instance? OWLLiteral e))

(defn named?
  "Return true if `e` is an instance of `OWLNamedObject`."
  [e]
  (instance? OWLNamedObject e))

(defn obj-prop-exp?
  "Return true if `e` is an instance of `OWLPropertyExpression`."
  [e]
  (instance? OWLObjectPropertyExpression e))

(defn ontology?
  "Return true if `e` is an instance of `OWLOntology`."
  [e]
  (instance? OWLOntology e))
