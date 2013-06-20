;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, 2013, Newcastle University

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



(ns tawny.render
  (:require [tawny.owl :as owl]
            [tawny.lookup]
            [tawny.util]
            )
  (:import
           (java.util Set)
           (org.semanticweb.owlapi.model
            OWLAnnotation
            OWLAnnotationProperty
            OWLAnnotationValue
            OWLClass
            OWLDataAllValuesFrom
            OWLDataExactCardinality
            OWLDataHasValue
            OWLDataMaxCardinality
            OWLDataMinCardinality
            OWLDataProperty
            OWLDataSomeValuesFrom
            OWLFacetRestriction
            OWLIndividual
            OWLLiteral
            OWLObjectAllValuesFrom
            OWLObjectComplementOf
            OWLObjectExactCardinality
            OWLObjectHasSelf
            OWLObjectHasValue
            OWLObjectIntersectionOf
            OWLObjectMaxCardinality
            OWLObjectMinCardinality
            OWLObjectOneOf
            OWLObjectSomeValuesFrom
            OWLObjectUnionOf
            OWLObjectProperty
            OWLProperty
            )
           (org.semanticweb.owlapi.vocab
            OWLFacet
            OWL2Datatype)
           ))


(defn named-entity-as-string [entity]
  (-> entity
      (.getIRI)
      (.toURI)
      (.toString)))

(defn ontologies []
  (.getOntologies owl/owl-ontology-manager))

(defn setmap [f c]
  (apply clojure.set/union (map f c)))

(declare form)

(defmulti as-form class)

(defmethod as-form OWLClass [c]
  (binding [tawny.lookup/all-iri-to-var-cache
            (tawny.lookup/all-iri-to-var)]
    (let [ont (ontologies)
          super (.getSuperClasses c ont)
          equiv (.getEquivalentClasses c ont)
          disjoint (.getDisjointClasses c ont)
          annotation
          (setmap
           #(.getAnnotations c %) ont)
          cls (form c)
          ]
     `(
        ;; seems like a nice idea, but cls is always a symbol because form
        ;; OWLClass makes it so. Resolve-entity always returns a string, but
        ;; we don't know what kind -- a var string or an IRI?
        ~(if (symbol? cls)
           'defclass
           'owlclass)


         ~(form c)
         ~@(when (< 0 (count super))
             (cons
              :subclass
              (form super)))
         ~@(when (< 0 (count equiv))
             (cons
              :equivalent
              (form equiv)))
         ~@(when (< 0 (count disjoint))
             (cons
              :disjoint
              (form disjoint)))
         ~@(when (< 0 (count annotation))
             (cons :annotation
                   (form annotation)))
         ))))


;; subproperty chain from Ontology it would appear!
;; if (!isFiltered(AxiomType.SUB_PROPERTY_CHAIN_OF)) {
;;     for (OWLOntology ontology : getOntologies()) {
;;         for (OWLSubPropertyChainOfAxiom ax : ontology.getAxioms(AxiomType.SUB_PROPERTY_CHAIN_OF)) {
;;             if (ax.getSuperProperty().equals(property)) {
;;                 if (isDisplayed(ax)) {
;;                     SectionMap map = new SectionMap();
;;                     map.add(ax.getPropertyChain(), ax);
;;                     writeSection(SUB_PROPERTY_CHAIN, map, " o ", false, ontology);
;;                     axioms.add(ax);
;;                 }
;;             }
;;         }
;;     }
;; }


(defmethod as-form OWLObjectProperty [p]
  (binding [tawny.lookup/all-iri-to-var-cache
            (tawny.lookup/all-iri-to-var)]
    (let [ont (ontologies)
          domain (.getDomains p ont)
          range (.getRanges p ont)
          inverseof (.getInverses p ont)
          subpropertyof (.getSuperProperties p ont)
          characteristic
          (filter identity
                  (list
                   (and
                    (.isTransitive p ont)
                    'transitive)
                   (and
                    (.isFunctional p ont)
                    'functional)
                   (and
                    (.isInverseFunctional p ont)
                    'inversefunctionl)
                   (and
                    (.isSymmetric p ont)
                    'symmetric)
                   (and
                    (.isAsymmetric p ont)
                    'asymmetric)
                   (and
                    (.isIrreflexive p ont)
                    'irreflexive)
                   (and
                    (.isReflexive p ont)
                    'reflexive)
                   ))
          prop (form p)]

      `(
        ~(if (symbol? prop)
           'defoproperty
           'object-property)
        ~prop
         ~@(when (< 0 (count domain))
             (cons :domain
                   (form domain)))
         ~@(when (< 0 (count range))
             (cons :range
                   (form range)))
         ~@(when (< 0 (count inverseof))
             (cons :inverseof
                   (form inverseof)))
         ~@(when (< 0 (count characteristic))
             (cons :characteristics
                   characteristic))))))

(defmethod as-form OWLIndividual [p]
  (binding [tawny.lookup/all-iri-to-var-cache
            (tawny.lookup/all-iri-to-var)]
    (let [ont (ontologies)
          types (.getTypes p ont)
          same (setmap #(.getSameIndividuals p %) ont)
          diff (setmap #(.getDifferentIndividuals p %) ont)
          fact (apply clojure.set/union
                      (map #(.getObjectPropertyValues p %) ont))
          factnot
          (apply clojure.set/union
                 (map #(.getNegativeObjectPropertyValues p %) ont))
          ]
      `(defindividual ~(form p)
         ~@(when (< 0 (count types))
             (cons :type
                   (form types)))
         ~@(when (< 0 (count same))
             (cons :same
                   (form same)))
         ~@(when (< 0 (count diff))
             (cons :different
                   (form diff)))
         ~@(when (some
                  #(< 0 (count %))
                  [fact factnot])
             (concat
              [:fact]
              (form [:fact fact])
              (form [:factnot factnot])
              ))))))



(defmethod as-form OWLDataProperty [p]
  (binding [tawny.lookup/all-iri-to-var-cache
            (tawny.lookup/all-iri-to-var)]
    (let [ont (ontologies)
          domain (.getDomains p ont)
          range (.getRanges p ont)
          subpropertyof (.getSuperProperties p ont)
          characteristic
          (filter identity
                  (list
                   (and
                    (.isFunctional p ont)
                    'functional)
                   ))
          prop (form p)]

      `(
        ~(if (symbol? prop)
           'defdproperty
           'data-property)
        ~prop
         ~@(when (< 0 (count domain))
             (cons :domain
                   (form domain)))
         ~@(when (< 0 (count range))
             (cons :range
                   (form range)))
         ~@(when (< 0 (count characteristic))
             (cons :characteristics
                   characteristic))))))

(defmethod as-form OWLAnnotationProperty [p]
    (binding [tawny.lookup/all-iri-to-var-cache
            (tawny.lookup/all-iri-to-var)]
    (let [ont (ontologies)
          super
          (setmap #(.getSuperProperties p %) ont)
          ann
          (setmap #(.getAnnotations p %) ont)]
      `(defannotationproperty ~(form p)
         ~@(when (< 0 (count super))
             (cons :subproperty
                   (form super)))
         ~@(when (< 0 (count ann))
             (cons :annotations
                   (form ann)))))))

(defmethod as-form org.semanticweb.owlapi.model.OWLDatatype [d]
  ;; I think we can safely ignore this here -- if it declared, then it should
  ;; be used elsewhere also. I think. Regardless, there is no read syntax in
  ;; tawny at the moment.
  )

(defmethod as-form :default [p]
  (println "Unknown element in signature")
  (. Thread dumpStack)
  '(unknown as-form))


(defmulti form class)

;; how to get from {:a {1 2}} {:b {3 4}}
;; to [:a 1][:a 2]
;; or support (fact I1 I2)?

(defmethod form clojure.lang.IPersistentVector [v]
  (let [f (symbol (name (first v)))]
    (for [[ope ind]
          (reduce
           concat
           (for [[k v] (second v)]
             (for [x v]
               [k x])))]
      `(~f ~(form ope) ~(form ind)))))


(defmethod form Set [s]
  ;; no lazy -- we are going to render the entire form anyway, and we are
  ;; using a dynamic binding to cache the iri-to-var map. Lazy eval will break
  ;; this big time.
  (tawny.util/domap form s))

(defn- entity-or-iri [c]
  (let [res (tawny.lookup/resolve-entity c)]
    (if res
      (symbol
       (tawny.lookup/resolve-entity c))
      `(~'iri ~(tawny.lookup/named-entity-as-string c)))))

(defmethod form OWLClass [c]
  (entity-or-iri c))

(defmethod form OWLProperty [p]
  (entity-or-iri p))

(defmethod form OWLIndividual [i]
  (entity-or-iri i))

(defmethod form OWLObjectSomeValuesFrom [s]
  (list 'owlsome
        (form (.getProperty s))
        (form (.getFiller s))))

(defmethod form OWLObjectUnionOf [u]
  `(owlor ~@(form (.getOperands u))))

(defmethod form OWLObjectIntersectionOf [c]
  `(owland ~@(form (.getOperands c))))

(defmethod form OWLObjectAllValuesFrom [a]
  (list 'owlall
        (form (.getProperty a))
        (form (.getFiller a))))

(defmethod form OWLObjectComplementOf [c]
  (list 'owlnot
        (form (.getOperand c))))

(defmethod form OWLObjectExactCardinality [c]
  (list 'exactly (.getCardinality c)
        (form (.getProperty c))
        (form (.getFiller c))))

(defmethod form OWLObjectMaxCardinality [c]
  (list 'atmost (.getCardinality c)
        (form (.getProperty c))
        (form (.getFiller c))))

(defmethod form OWLObjectMinCardinality [c]
  (list 'atleast (.getCardinality c)
        (form (.getProperty c))
        (form (.getFiller c))))

(defmethod form OWLAnnotation [a]
  (let [v (.getValue a)]
    (cond
     (.isLabel a)
     (list* 'label
           (form v))
     (.isComment a)
     (list* 'owlcomment
           (form v))
     :default
     (list*
      (form (.getProperty a))
      (form v)))))

(defmethod form OWLAnnotationProperty [p]
  (entity-or-iri p))

;; this can be improved somewhat -- not converting classes into something
;; readable.
(defmethod form OWLAnnotationValue [v]
  (list
   (.toString v)))

(defmethod form OWLLiteral [l]
  (cond
   (.isInteger l)
   (.parseInteger l)
   (.hasLang l)
   (list (.getLiteral l)
         (.getLang l))
   :default
   (list (.getLiteral l))))


(defmethod form OWLDataSomeValuesFrom [d]
  `(owlsome ~(form (.getProperty d))
            ~(form (.getFiller d))))


(defmethod form org.semanticweb.owlapi.model.OWLDatatypeRestriction [d]
  (for [fr (.getFacetRestrictions d)]
    `(span ~@(form fr))))


(defmethod form OWLFacetRestriction [d]
  `(~(form (.getFacet d)) ~(form (.getFacetValue d))))


(defmethod form OWLFacet [d]
  (get
       {OWLFacet/MAX_EXCLUSIVE '<
        OWLFacet/MAX_INCLUSIVE '<=
        OWLFacet/MIN_EXCLUSIVE '>
        OWLFacet/MIN_INCLUSIVE '>=
        }
       d))

(defmethod form org.semanticweb.owlapi.model.OWLDatatype [d]
  (form (.getBuiltInDatatype d)))

(defmethod form OWL2Datatype [d]
  (get
       {
        OWL2Datatype/RDF_PLAIN_LITERAL 'rdf:plainliteral
        OWL2Datatype/XSD_FLOAT 'xsd:float
        OWL2Datatype/XSD_DOUBLE 'xsd:double
        OWL2Datatype/XSD_INTEGER 'xsd:integer
        }
       d))


;; OWLObjectHasSelf
;; OWLObjectHasValue
;; OWLObjectOneOf

(defmethod form String [e]
  e)

(defmethod form :default [e]
  (do
    (println "Unknown form" (class e))
    (. Thread dumpStack)
    `(unknown form)))







;; OWLDataAllValuesFrom
;; OWLDataExactCardinality
;; OWLDataHasValue
;; OWLDataMaxCardinality
;; OWLDataMinCardinality
;; OWLDataSomeValuesFrom
;; OWLObjectAllValuesFrom
;; OWLObjectComplementOf
;; OWLObjectExactCardinality
;; OWLObjectHasSelf
;; OWLObjectHasValue
;; OWLObjectIntersectionOf
;; OWLObjectMaxCardinality
;; OWLObjectMinCardinality
;; OWLObjectOneOf
;; OWLObjectUnionOf



