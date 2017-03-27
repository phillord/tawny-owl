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
  (:require [tawny.protocol])
  (:import (org.semanticweb.owlapi.model
            IRI
            OWLAnonymousIndividual
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
            OWLObjectProperty
            OWLObjectPropertyExpression
            OWLOntology)))


(defn- entity-instance?
  "Return true if the entity of `e` is an instance of `type`."
  [type e]
  (instance? type (tawny.protocol/as-entity e)))

(defn iri?
  "Return true if `e` is an instance of `IRI`."
  [e]
  (entity-instance? IRI e))

(defn ann-val?
  "Return true if `e` is an instance of `OWLAnnotationValue`."
  [e]
  (entity-instance? OWLAnnotationValue e))

(defn ann-prop?
  "Return true if `e` is an instance of `OWLAnnotationProperty`."
  [e]
  (entity-instance? OWLAnnotationProperty e))

(defn anonymous?
  "Return true if `e` is an instance of `OWLAnonymousIndividual`."
  [e]
  (entity-instance? OWLAnonymousIndividual e))

(defn owl-class?
  "Return true if `e` is an instance of `OWLClass`."
  [e]
  (entity-instance? OWLClass e))

(defn class-exp?
  "Return true if `e` is an instance of `OWLClassExpression`."
  [e]
  (entity-instance? OWLClassExpression e))

(defn data-prop?
  "Return true if `e` is an instance of `OWLDataProperty`."
  [e]
  (entity-instance? OWLDataProperty e))

(defn data-prop-exp?
  "Return true if `e` is an instance of `OWLDataPropertyExpression`."
  [e]
  (entity-instance? OWLDataPropertyExpression e))

(defn data-range?
  "Return true if `e` is an instance of `OWLDataRange`."
  [e]
  (entity-instance? OWLDataRange e))

(defn data-type?
  "Return true if `e` is an instance of `OWLDatatype`."
  [e]
  (entity-instance? OWLDatatype e))

(defn individual?
  "Return true if `e` is an instance of `OWLIndividual`."
  [e]
  (entity-instance? OWLIndividual e))

(defn literal?
  "Return true if `e` is an instance of `OWLLiteral`."
  [e]
  (entity-instance? OWLLiteral e))

(defn named?
  "Return true if `e` is an instance of `OWLNamedObject`."
  [e]
  (entity-instance? OWLNamedObject e))

(defn obj-prop?
  "Return true if `e` is an instance of `OWLObjectProperty`."
  [e]
  (entity-instance? OWLObjectProperty e))

(defn obj-prop-exp?
  "Return true if `e` is an instance of `OWLObjectPropertyExpression`."
  [e]
  (entity-instance? OWLObjectPropertyExpression e))

(defn ontology?
  "Return true if `e` is an instance of `OWLOntology`."
  [e]
  (entity-instance? OWLOntology e))
