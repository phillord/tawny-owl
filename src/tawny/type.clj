(ns tawny.type
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


;; Object = obj
;; Property = prop
;; Expression = ex
;; Individual = ind
;; Literal
;; datatype
;; datarange
;; Annotation == ann
;; Class

(defn iri? [e]
  (instance? IRI e))

(defn ann-val? [e]
  (instance? OWLAnnotationValue e))

(defn ann-prop? [e]
  (instance? OWLAnnotationProperty e))

(defn owl-class? [e]
  (instance? OWLClass e))

(defn class-exp? [e]
  (instance? OWLClassExpression e))

(defn data-prop? [e]
  (instance? OWLDataProperty e))

(defn data-prop-exp? [e]
  (instance? OWLDataPropertyExpression e))

(defn data-range? [e]
  (instance? OWLDataRange e))

(defn data-type? [e]
  (instance? OWLDatatype e))

(defn individual? [e]
  (instance? OWLIndividual e))

(defn literal? [e]
  (instance? OWLLiteral e))

(defn named? [e]
  (instance? OWLNamedObject e))

(defn obj-prop-exp? [e]
  (instance? OWLObjectPropertyExpression e))

(defn ontology? [e]
  (instance? OWLOntology e))
