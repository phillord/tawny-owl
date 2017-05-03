;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2017, Newcastle University

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
    ^{:doc "Print OWL Objects nicely"
      :author "Phillip Lord"}
    tawny.print
  (:require [clojure.string])
  (:import
   [org.semanticweb.owlapi.model
    IRI
    OWLObject
    OWLOntology
    OWLNamedObject
    OWLNaryBooleanClassExpression
    OWLObjectComplementOf
    OWLQuantifiedRestriction]
   [java.io Writer]))

(defn- shorten-iri [^IRI iri]
  (str
   (or (.getFragment iri)
       iri)))

(defmethod print-method IRI [o ^Writer w]
  (.write w (str "#[iri " o "]")))

(defn- name-for-class [o]
  (some
   (fn [clazz]
     (when (isa? (.getClass ^Object o) clazz)
       (.substring
        (.getSimpleName ^Class clazz)
        3)))
   [org.semanticweb.owlapi.model.OWLObjectInverseOf
    org.semanticweb.owlapi.model.OWLDataHasValue
    org.semanticweb.owlapi.model.OWLObjectHasSelf
    org.semanticweb.owlapi.model.OWLObjectHasValue
    org.semanticweb.owlapi.model.OWLDatatype
    org.semanticweb.owlapi.model.OWLFacetRestriction
    org.semanticweb.owlapi.model.OWLDatatypeRestriction
    org.semanticweb.owlapi.model.OWLDataOneOf
    org.semanticweb.owlapi.model.OWLDataMinCardinality
    org.semanticweb.owlapi.model.OWLDataMaxCardinality
    org.semanticweb.owlapi.model.OWLDataExactCardinality
    org.semanticweb.owlapi.model.OWLDataIntersectionOf
    org.semanticweb.owlapi.model.OWLDataUnionOf
    org.semanticweb.owlapi.model.OWLDataComplementOf
    org.semanticweb.owlapi.model.OWLDataAllValuesFrom
    org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
    org.semanticweb.owlapi.model.OWLOntology
    org.semanticweb.owlapi.model.OWLLiteral
    org.semanticweb.owlapi.model.OWLAnnotationValue
    org.semanticweb.owlapi.model.OWLAnnotationProperty
    org.semanticweb.owlapi.model.OWLAnnotation
    org.semanticweb.owlapi.model.OWLObjectMinCardinality
    org.semanticweb.owlapi.model.OWLObjectMaxCardinality
    org.semanticweb.owlapi.model.OWLObjectExactCardinality
    org.semanticweb.owlapi.model.OWLObjectComplementOf
    org.semanticweb.owlapi.model.OWLObjectAllValuesFrom
    org.semanticweb.owlapi.model.OWLObjectIntersectionOf
    org.semanticweb.owlapi.model.OWLObjectUnionOf
    org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom
    org.semanticweb.owlapi.model.OWLObjectOneOf
    org.semanticweb.owlapi.model.OWLIndividual
    org.semanticweb.owlapi.model.OWLProperty
    org.semanticweb.owlapi.model.OWLClass
    org.semanticweb.owlapi.model.OWLOntology

    org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom
    org.semanticweb.owlapi.model.OWLAnnotationAxiom
    org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom
    org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom
    org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLClassAssertionAxiom
    org.semanticweb.owlapi.model.OWLClassAxiom
    org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom
    org.semanticweb.owlapi.model.OWLDataPropertyAxiom
    org.semanticweb.owlapi.model.OWLDataPropertyCharacteristicAxiom
    org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom
    org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom
    org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom
    org.semanticweb.owlapi.model.OWLDeclarationAxiom
    org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom
    org.semanticweb.owlapi.model.OWLDisjointClassesAxiom
    org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom
    org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom
    org.semanticweb.owlapi.model.OWLDisjointUnionAxiom
    org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
    org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom
    org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom
    org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom
    org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLHasKeyAxiom
    org.semanticweb.owlapi.model.OWLIndividualAxiom
    org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom
    org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLLogicalAxiom
    org.semanticweb.owlapi.model.OWLNaryAxiom
    org.semanticweb.owlapi.model.OWLNaryClassAxiom
    org.semanticweb.owlapi.model.OWLNaryIndividualAxiom
    org.semanticweb.owlapi.model.OWLNaryPropertyAxiom
    org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom
    org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom
    org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom
    org.semanticweb.owlapi.model.OWLObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLObjectPropertyCharacteristicAxiom
    org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom
    org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom
    org.semanticweb.owlapi.model.OWLPropertyAssertionAxiom
    org.semanticweb.owlapi.model.OWLPropertyAxiom
    org.semanticweb.owlapi.model.OWLPropertyDomainAxiom
    org.semanticweb.owlapi.model.OWLPropertyRangeAxiom
    org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLSameIndividualAxiom
    org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom
    org.semanticweb.owlapi.model.OWLSubClassOfAxiom
    org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom
    org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom
    org.semanticweb.owlapi.model.OWLSubPropertyAxiom
    org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom
    org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom
    org.semanticweb.owlapi.model.OWLUnaryPropertyAxiom
    ]))

(defn- join-seq [s]
  (clojure.string/join " " (map pr-str s)))

;; (remove-method print-method OWLObject)
(defmethod print-method OWLObject [^OWLObject o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x signature %s]"
    (name-for-class o)
    (System/identityHashCode o)
    (clojure.string/join
     " "
     (map
      pr-str
      (remove
       #(= o %)
       (.getSignature o))))
    "]")))

(defn- print-short [o]
  (if (instance? OWLNamedObject o)
    (shorten-iri (.getIRI ^OWLNamedObject o))
    (pr-str o)))

(defmethod print-method OWLQuantifiedRestriction [^OWLQuantifiedRestriction o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x %s %s]"
    (name-for-class o)
    (System/identityHashCode o)
    (print-short (.getProperty o))
    (print-short (.getFiller o)))))

(defmethod print-method OWLNaryBooleanClassExpression
  [^OWLNaryBooleanClassExpression o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x %s]"
    (name-for-class o)
    (System/identityHashCode o)
    (clojure.string/join
     " "
     (map
      #(print-short %)
      (.getOperands o)))
    "]")))

(defmethod print-method OWLObjectComplementOf
  [^OWLObjectComplementOf o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x %s]"
    (name-for-class o)
    (System/identityHashCode o)
    (print-short (.getOperand o))
    "]")))

(defmethod print-method OWLNamedObject [^OWLNamedObject o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x %s]"
    (name-for-class o)
    (System/identityHashCode o)
    (shorten-iri
     (.getIRI o)))))

(defmethod print-method OWLOntology [^OWLOntology o ^Writer w]
  (.write
   w
   (format
    "#[%s 0x%x %s %s:%s]"
    (name-for-class o)
    (System/identityHashCode o)
    (shorten-iri
     (.get
      (.getOntologyIRI
       (.getOntologyID o))))
    (.getAxiomCount o)
    (.getLogicalAxiomCount o))))
