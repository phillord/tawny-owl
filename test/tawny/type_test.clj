(ns tawny.type-test
  (:require [tawny.type :as tt]
            [tawny.owl :as o]
            [clojure.test :refer :all])
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


(deftest iri-is-type
  (is
   (tt/iri? (o/iri "http://example.com")))
  (is
   (tt/iri?
    (o/annotate
     (o/iri "http://example.com")
     nil))))
