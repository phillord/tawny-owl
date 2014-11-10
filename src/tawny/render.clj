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
(ns
    ^{:doc "Renders OWL Entities into tawny.owl forms"
      :author "Phillip Lord"}
  tawny.render
  (:require [tawny.owl :as owl]
            [tawny.lookup]
            [tawny.util]
            [clojure.set])
  (:import
           (java.util Set)
           (org.semanticweb.owlapi.model
            OWLAnnotation
            OWLAnnotationProperty
            OWLAnnotationValue
            OWLClass
            OWLDataAllValuesFrom
            OWLDataComplementOf
            OWLDataExactCardinality
            OWLDataHasValue
            OWLDataIntersectionOf
            OWLDataMaxCardinality
            OWLDataMinCardinality
            OWLDataOneOf
            OWLDataProperty
            OWLDataSomeValuesFrom
            OWLDatatype
            OWLDatatypeRestriction
            OWLDataUnionOf
            OWLFacetRestriction
            OWLIndividual
            OWLLiteral
            OWLNamedIndividual
            OWLNamedObject
            OWLObjectAllValuesFrom
            OWLObjectComplementOf
            OWLObjectExactCardinality
            OWLObjectHasSelf
            OWLObjectHasValue
            OWLObjectIntersectionOf
            OWLObjectInverseOf
            OWLObjectMaxCardinality
            OWLObjectMinCardinality
            OWLObjectOneOf
            OWLObjectSomeValuesFrom
            OWLObjectUnionOf
            OWLObjectProperty
            OWLOntology
            OWLProperty
            OWLSubPropertyChainOfAxiom
            )
           (org.semanticweb.owlapi.vocab
            OWLFacet
            OWL2Datatype)
           ))


;; :owl-some
;; :only
;; :owl-and
;; :owl-or
;; :exactly
;; :oneof
;; :at-least
;; :at-most
;; :has-value
;; :owl-not
;; :span
;; :iri
;; :label
;; :comment
;; :annotation
;; :literal
;; :<
;; :<=S
;; :>
;; :>=
;; :has-self
;; :inverse

(def ^{:private true}
  unnamed-entity-map
  {
   [:explicit :keyword]
   {
    :object-some :object-some
    :object-only :object-only
    :object-and :object-and
    :object-or :object-or
    :object-exactly :object-exactly
    :object-oneof :object-oneof
    :object-at-least :object-at-least
    :object-at-most :object-at-most
    :object-has-value :object-has-value
    :object-not :object-not

    ;; object only so no explicit form
    :object-has-self :has-self
    :object-inverse :inverse

    :data-some :data-some
    :data-only :data-only
    :data-and :data-and
    :data-or :data-or
    :data-exactly :data-exactly
    :data-oneof :data-oneof
    :data-at-least :data-at-least
    :data-at-most :data-at-most
    :data-has-value :data-has-value
    :data-not :data-not

    :object-fact :object-fact
    :object-fact-not :object-fact-not
    :data-fact :data-fact
    :data-fact-not :data-fact-not

    :span :span
    :iri :iri
    :label :label
    :comment :comment
    :annotation :annotation
    :literal :literal
    :< :<
    :<= :<=
    :> :>
    :>= :>=
    :>< :><
    :>=< :>=<
    }

   [:keyword]
   {
    :object-some :some
    :object-only :only
    :object-and :and
    :object-or :or
    :object-exactly :exactly
    :object-oneof :oneof
    :object-at-least :at-least
    :object-at-most :at-most
    :object-has-value :has-value
    :object-not :not

    :object-has-self :has-self
    :object-inverse :inverse

    :data-some :some
    :data-only :only
    :data-and :and
    :data-or :or
    :data-exactly :exactly
    :data-oneof :oneof
    :data-at-least :at-least
    :data-at-most :at-most
    :data-has-value :has-value
    :data-not :not

    :object-fact :fact
    :object-fact-not :fact-not
    :data-fact :fact
    :data-fact-not :fact-not


    :span :span
    :iri :iri
    :label :label
    :comment :comment
    :annotation :annotation
    :literal :literal
    :< :<
    :<= :<=
    :> :>
    :>= :>=
    :>< :><
    :>=< :>=<
    }

   [:explicit]
   {
    :object-some 'object-some
    :object-only 'object-only
    :object-and 'object-and
    :object-or 'object-or
    :object-exactly 'object-exactly
    :object-oneof 'object-oneof
    :object-at-least 'object-at-least
    :object-at-most 'object-at-most
    :object-has-value 'object-has-value
    :object-not 'object-not

    ;; these are object only -- so no explicit form
    :object-has-self 'has-self
    :object-inverse 'inverse

    :data-some 'data-some
    :data-only 'data-only
    :data-and 'data-and
    :data-or 'data-or
    :data-exactly 'data-exactly
    :data-oneof 'data-oneof
    :data-at-least 'data-at-least
    :data-at-most 'data-at-most
    :data-has-value 'data-has-value
    :data-not 'data-not


    ;; we don't have a public explicit syntax for facts yet, which is probably
    ;; not ideal
    :object-fact 'fact
    :object-fact-not 'fact-not
    :data-fact 'fact
    :data-fact-not 'fact-not

    :span 'span
    :iri 'iri
    :label 'label
    :comment 'owl-comment
    :annotation 'annotation
    :literal 'literal
    :< '<
    :<= '<=
    :> '>
    :>= '>=
    :>< '><
    :>=< '>=<
    }

   []
   {
    :object-some 'owl-some
    :object-only 'only
    :object-and 'owl-and
    :object-or 'owl-or
    :object-exactly 'exactly
    :object-oneof 'oneof
    :object-at-least 'at-least
    :object-at-most 'at-most
    :object-has-value 'has-value
    :object-not 'owl-not
    :object-has-self 'has-self
    :object-inverse 'inverse

    :data-some 'owl-some
    :data-only 'only
    :data-and 'owl-and
    :data-or 'owl-or
    :data-exactly 'exactly
    :data-oneof 'oneof
    :data-at-least 'at-least
    :data-at-most 'at-most
    :data-has-value 'has-value
    :data-not 'owl-not
    :data-has-self 'has-self
    :data-inverse 'inverse

    :object-fact 'fact
    :object-fact-not 'fact-not
    :data-fact 'fact
    :data-fact-not 'fact-not

    :span 'span
    :iri 'iri
    :label 'label
    :comment 'owl-comment
    :annotation 'annotation
    :literal 'literal
    :< '<
    :<= '<=
    :> '>
    :>= '>=
    :>< '><
    :>=< '>=<}})

(def
  ^{:private true}
  named-entity-map
  {OWLClass [:class 'defclass 'owl-class]
   OWLObjectProperty [:oproperty 'defoproperty 'object-property]
   OWLNamedIndividual [:individual 'defindividual 'individual]
   OWLDataProperty [:dproperty 'defdproperty 'datatype-property]
   OWLAnnotationProperty [:aproperty 'defaproperty 'annotation-property]
   OWLOntology [:ontology 'defontology 'ontology]})

(defn- unnamed-entity [entity-keyword options]
  (get
   (get options ::unnamed)
   entity-keyword))

(defn- named-entity [type entity options]
  (cond
   (get options :keyword)
   (first (get named-entity-map type))
   (and
    (= :resolve
       (get options :terminal))
    (tawny.lookup/resolve-entity entity))
   (second (get named-entity-map type))
   :else
   (nth (get named-entity-map type) 2)))

(defn- named-entity-as-string
  "Return a string identifier for an entity"
  [^OWLNamedObject entity]
  (-> entity
      (.getIRI)
      (.toURI)
      (.toString)))

(defn- ^java.util.Set ontologies
  "Fetch all known ontologies."
  [options]
  (or (get options :ontologies)
      (and (get options :manager)
           (.getOntologies
            ^org.semanticweb.owlapi.model.OWLOntologyManager
            (get options :manager)))
      (.getOntologies
       (owl/owl-ontology-manager))))

(defn- setmap
  "Apply f to list c, union the results."
  [f c]
  (apply clojure.set/union (map f c)))

(declare form)

(defn- class-compare
  "Compares two classes placing subclasses first."
  [this that]
  (cond
   (isa? this that)
   -1
   (isa? that this)
   1
   :default 0))

(defn- as-form-lookup
  "Returns the class of the OWLEntity of which this is a subclass."
  [c]
  (first
   (filter
    (fn as-form-lookup [parent]
      (isa? c parent))
    (list OWLClass OWLObjectProperty OWLNamedIndividual
          OWLDataProperty OWLAnnotationProperty OWLDatatype
          OWLOntology))))

(defmulti ^{:private true} as-form-int
  (let
      ;; memoize because this is limited by the number of classes
      ;; and it limits isa? lookup
      [l (memoize as-form-lookup)]
    (fn [c options]
      (l (class c)))))

;; so, we can parse both OWLOntology okay
;; can do this generically, because we can now put an arbitaray ontology annotation
;; because annotations are maybe ontology functions.
;;
;; This leaves imports as the only unrendered things. Currently, the read
;; forms are separate from the ontology themselves, and there is no OWLImport
;; mechanism. So, we might want to fix this, by allowing imports to be
;; declared on the ontology form.
(defmethod as-form-int OWLOntology
  [^OWLOntology o options]
  (let [lst (if (get options :keyword)
              list list*)]
    (concat
     (list
      (named-entity OWLOntology o options))
     (when (:keyword options)
       (list (form o options)))

     (list :iri
           (.. o getOntologyID getOntologyIRI toString))

     (let [viri (.. o getOntologyID getVersionIRI)]
       (when viri
         (list :viri (str viri))))
     ;; what to do about noname? guess we pass it in as an option
     ;; or we could check existing options
     (when-let [f (.getOntologyFormat (tawny.owl/owl-ontology-manager) o)]
       (when (.isPrefixOWLOntologyFormat f)
         (when-let [pre (tawny.owl/get-prefix o)]
           (list :prefix
                 ;; chop off the colon
                 (.substring pre 0
                             (dec (.length pre)))))))
     ;; imports
     (when-let [imp-decl
                (seq (.getImportsDeclarations o))]
       (lst
        :import
        (form (map #(.getIRI
                     ^org.semanticweb.owlapi.model.OWLImportsDeclaration
                     %) imp-decl) options)))
     (when-let [ann
                (seq (.getAnnotations o))]
       (lst
        :annotation
        (form ann options))))))

(defmethod as-form-int OWLClass
  [^OWLClass c options]
  (let [ont (ontologies options)
        super (.getSuperClasses c ont)
        equiv (.getEquivalentClasses c ont)
        disjoint (.getDisjointClasses c ont)
        annotation
        (setmap
         #(.getAnnotations c %) ont)
        cls (form c options)
        ;; If we have :keywords each frame has a list after is, so just use
        ;; list to make it. If we are returning symbols then we don't want a
        ;; raw elements, so we use list*, which gives us the equivalent of a
        ;; splice.
        lst (if (get options :keyword)
              list
              list*)]
    (concat
     (list
      (named-entity OWLClass c options)
      (form c options))

     (when (pos? (count super))
       (lst :super
            (form super options)))

     (when (pos? (count equiv))
       (lst
        :equivalent
        (form equiv options)))

     (when (pos? (count disjoint))
       (lst
        :disjoint
        (form disjoint options)))

     (when (pos? (count annotation))
       (lst :annotation
            (form annotation options))))))

(defmethod as-form-int OWLObjectProperty
  [^OWLObjectProperty p options]
  (let [ont (ontologies options)
        domain (.getDomains p ont)
        range (.getRanges p ont)
        inverseof (.getInverses p ont)
        superprop (.getSuperProperties p ont)
        subchainaxioms
        ;; only the ones associated with this property
        (filter
         #(= p (.getSuperProperty ^OWLSubPropertyChainOfAxiom %))
         ;; to one set
         (apply clojure.set/union
                ;; get all SUB_PROPERTY_CHAIN_OF axioms in all ontologies
                ;; each as set
                (map
                 #(set
                   (.getAxioms
                    ^OWLOntology %
                    org.semanticweb.owlapi.model.AxiomType/SUB_PROPERTY_CHAIN_OF))
                 ont)))
        annotation
        (setmap
         #(.getAnnotations p %) ont)
        characteristic
        (filter identity
                (list
                 (and
                  (.isTransitive p ont)
                  :transitive)
                 (and
                  (.isFunctional p ont)
                  :functional)
                 (and
                  (.isInverseFunctional p ont)
                  :inversefunctional)
                 (and
                  (.isSymmetric p ont)
                  :symmetric)
                 (and
                  (.isAsymmetric p ont)
                  :asymmetric)
                 (and
                  (.isIrreflexive p ont)
                  :irreflexive)
                 (and
                  (.isReflexive p ont)
                  :reflexive)
                 ))
        lst (if (get options :keyword)
              list
              list*)
        ]
    (concat
     (list (named-entity OWLObjectProperty p options)
           (form p options))

     (when (pos? (count superprop))
       (lst :super
            (form superprop options)))

     (when (pos? (count subchainaxioms))
       (lst :subchain
            (form
             ;; get the all the chains
             (map #(vec (.getPropertyChain
                         ^OWLSubPropertyChainOfAxiom %)) subchainaxioms)
             options)))
     (when (pos? (count domain))
       (lst :domain
            (form domain options)))
     (when (pos? (count range))
       (lst :range
            (form range options)))
     (when (pos? (count inverseof))
       (lst :inverse
            (form inverseof options)))
     (when (pos? (count characteristic))
       (lst :characteristic
            characteristic))
     (when (pos? (count annotation))
       (lst :annotation
            (form annotation options))))))

(defmethod as-form-int OWLNamedIndividual
  [^OWLNamedIndividual p options]
  (let [ont (ontologies options)
        types (.getTypes p ont)
        same (setmap #(.getSameIndividuals p %) ont)
        diff (setmap #(.getDifferentIndividuals p %) ont)
        annotation
        (setmap
         (fn [^OWLOntology o] (.getAnnotations p o)) ont)
        facts
        (remove
         nil?
         (list
          (let [fs
                (into {} (setmap #(.getObjectPropertyValues p %) ont))]
            (when (seq fs)
              {::type :object-fact :facts fs}))
          (let [fs
                (into {} (setmap #(.getDataPropertyValues p %) ont))]
            (when (seq fs)
              {::type :data-fact :facts fs}))
          (let [fs
                (into {} (setmap #(.getNegativeObjectPropertyValues p %) ont))]
            (when (seq fs)
              {::type :object-fact-not :facts fs}))
          (let [fs
                (into {} (setmap #(.getNegativeDataPropertyValues p %) ont))]
            (when (seq fs)
              {::type :data-fact-not :facts fs}))))
        ind (form p options)
        lst (if (get options :keyword)
              list
              list*)]
    (concat
     (list
      (named-entity OWLNamedIndividual p options)
      (form p options))
     (when (pos? (count types))
       (lst :type
            (form types options)))
     (when (pos? (count same))
       (lst :same
            (form same options)))
     (when (pos? (count diff))
       (lst :different
            (form diff options)))
     (when (pos? (count annotation))
       (lst :annotation
            (form annotation options)))
     ;; what is going on here!
     (when (seq facts)
       (lst
        :fact
        ;; always a list!
        (apply concat (form facts options)))))))

(defmethod as-form-int OWLDataProperty
  [^OWLDataProperty p options]
  (let [ont (ontologies options)
        domain (.getDomains p ont)
        range (.getRanges p ont)
        superprop (.getSuperProperties p ont)
        annotation
        (setmap
         #(.getAnnotations p %) ont)
        characteristic
        (filter identity
                (list
                 (and
                  (.isFunctional p ont)
                  :functional)
                 ))
        prop (form p options)
        lst (if (get options :keyword)
              list
              list*)]
    (concat
     (list
      (named-entity OWLDataProperty p options)
      prop)
     (when (pos? (count superprop))
       (lst :super
             (form superprop options)))
     (when (pos? (count domain))
       (lst :domain
            (form domain options)))
     (when (pos? (count range))
       (lst :range
            (form range options)))
     (when (pos? (count characteristic))
       (lst :characteristic
            characteristic))
     (when (pos? (count annotation))
       (lst :annotation
            (form annotation options))))))

(defmethod as-form-int OWLAnnotationProperty
  [^OWLAnnotationProperty p options]
  (let [ont (ontologies options)
        super
        (setmap (fn [^OWLOntology o] (.getSuperProperties p o)) ont)
        ann
        (setmap #(.getAnnotations p %) ont)
        prop (form p options)
        lst (if (get options :keyword)
              list
              list*)]
    (concat
     (list
      (named-entity OWLAnnotationProperty p options)
      (form p options))
     (when (pos? (count super))
       (lst :super
            (form super options)))
     (when (pos? (count ann))
       (lst :annotation
            (form ann options))))))

(defmethod as-form-int org.semanticweb.owlapi.model.OWLDatatype [f options]
  ;; Really uncertain about this at the moment -- this used to be empty and I
  ;; am worried that I have messed something up by adding it. Only time we
  ;; tell here.
  ;;
  ;; Think this is wrong now  because we do have a read form and should be
  ;; producing that.
  ;;
  ;; It' definately fucked for :keyword rendering -- and also comes up with
  ;; inbuilt datatypes like xsd_string.
  (form f options))

(defn- branch?
  "Returns true if we are out a branch of a form tree."
  [elem]
  (and (sequential? elem)
       (every? sequential? elem)))

(defn- semi-flatten
  "Flattens a list except for lists that contain no other lists."
  [s]
  (if (branch? s)
    (mapcat semi-flatten s)
    (list s)))

(defmethod as-form-int :default [f options]
  (form f options)
  (let [k
        ;; many forms return lists, so we flatten here if we can to a single
        ;; element list or we get lots of confusing sublists
        (semi-flatten
         (form f options))]
    ;; as a special case if there is only one element in the list unpack that too
    (if (next k)
      k
      (first k))))

(defn as-form
  "Given a OWLObject render it to a Tawny form, or clojure data structure.
Entity can be any OWLObject or a number of Clojure collection types. The
rendered form is controlled with options, which are interpreted as a map; by
default an evalable Tawny-OWL form is returned.
:keyword true renders entities as keywords rather than symbols. This also affects
the nesting structure -- frames are returned as an explicit rather than
implicit list.
:explicit true returns data- or object- symbols or keywords; for
example, :object-and or :data-and will be returned rather than :and.
:terminal :resolve returns terminal objects as their interned symbol (if one exists) or
their IRI.
:terminal :object returns terminal objects as their OWL API object."
  [entity & options]
  (let [options (apply hash-map options)
        unnamed
        (get
         unnamed-entity-map
         (filter
          identity
          [(and (get options :explicit)
                :explicit)
           (and (get options :keyword)
                :keyword)]))
        terminal
        (get options :terminal :resolve)
        options
        (merge
         {::unnamed unnamed
          ::terminal terminal}
         options)]
    (as-form-int entity options)))

(def
  ^{:priate true
    :doc "All the classes that form can render, sorted to include most
specific first."}
  form-lookup-list
  (sort class-compare
        (list clojure.lang.ISeq Set java.util.Map clojure.lang.IPersistentVector
              org.semanticweb.owlapi.model.IRI OWLClass OWLProperty
              OWLIndividual OWLObjectOneOf OWLObjectSomeValuesFrom OWLObjectUnionOf
              OWLObjectIntersectionOf OWLObjectAllValuesFrom OWLObjectComplementOf
              OWLObjectExactCardinality OWLObjectMaxCardinality OWLObjectMinCardinality
              OWLAnnotation OWLAnnotationProperty
              OWLAnnotationValue OWLLiteral OWLOntology
              OWLDataSomeValuesFrom OWLDataAllValuesFrom OWLDataComplementOf
              OWLDataUnionOf OWLDataIntersectionOf OWLDataExactCardinality
              OWLDataMaxCardinality OWLDataMinCardinality OWLDataOneOf
              OWLDatatypeRestriction OWLFacetRestriction
              org.semanticweb.owlapi.model.OWLDatatype
              org.semanticweb.owlapi.model.OWLObjectHasValue
              org.semanticweb.owlapi.model.OWLObjectHasSelf
              org.semanticweb.owlapi.model.OWLDataHasValue
              OWLObjectInverseOf String)))


(defn- form-lookup
  "Returns first class from form-lookup-list that returns true
of isa?"
  [c]
  (first
   (filter
    (fn form-lookup [parent]
      (isa? c parent))
    form-lookup-list)))

(defmulti
  ^{:private true
    :doc "Renders an OWL Entity (or some collections) to tawny forms."}
  form
  (let
      ;; memoize because this is limited by the number of classes
      ;; and it limits isa? lookup
      [l (memoize form-lookup)]
    (fn [c options]
      (l (class c)))))

(defmethod form clojure.lang.ISeq [s options]
  (doall (map #(form % options) s)))

(defmethod form Set [s options]
  (map #(form % options) s))

(defmethod form clojure.lang.IPersistentVector
  [v options]
  ;; return preversing vector
  (vec (map #(form % options) v)))

(defn form-fact
  "Renders facts which as-form-int passes as a map with a
::type key for positive or negative fact."
  [s options]
  (let [type-sym (unnamed-entity (::type s) options)]
    (mapcat
     (fn create-fact-forms
       [property]
       (map list
         (repeat type-sym)
         (repeat (form property options))
         (map #(form % options)
              ;; the OWLAPI returns the facts as a map from a property to a
              ;; set of individuals or literals, so here we unwind the set
              (seq (get (:facts s) property)))))
     ;; each of the properties
     (keys (:facts s)))))

(defmethod form java.util.Map [m options]
  (if (get m ::type)
    ;; maps are mostly collections, but we also use them to transfer facts
    (form-fact m options)
    (for
        [[k v] m]
      `(~(form k options) ~(form v options)))))

(defn- entity-or-iri
  "Return either the interned var holding an entity, or an IRI,
depending on the value of *terminal-strategy*"
  [c options]
  (case (get options ::terminal)
    :object c
    :iri
    (list
     (unnamed-entity :iri options)
     (tawny.lookup/named-entity-as-string c))
    :resolve
    (if-let [res (tawny.lookup/resolve-entity c)]
      (symbol
       (tawny.lookup/resolve-entity c))
      (entity-or-iri
       c
       (merge options {::terminal :iri})))))

(defmethod form OWLClass [c options]
  (entity-or-iri c options))

(defmethod form OWLProperty [p options]
  (entity-or-iri p options))

(defmethod form OWLIndividual [i options]
  (entity-or-iri i options))

(defmethod form OWLOntology [o options]
  (entity-or-iri o options))

(defmethod form OWLObjectOneOf
  [^OWLObjectOneOf o options]
  (list*
   (unnamed-entity :object-oneof options)
   (form (.getIndividuals o) options)))

(defmethod form OWLObjectSomeValuesFrom
  [^OWLObjectSomeValuesFrom s options]
  (list
   (unnamed-entity :object-some options)
   (form (.getProperty s) options)
   (form (.getFiller s) options)))

(defmethod form OWLObjectUnionOf
  [^OWLObjectUnionOf u options]
   (list*
    (unnamed-entity :object-or options)
    (form (.getOperands u) options)))

(defmethod form OWLObjectIntersectionOf
  [^OWLObjectIntersectionOf c options]
  (list*
   (unnamed-entity :object-and options)
   (form (.getOperands c) options)))

(defmethod form OWLObjectAllValuesFrom
  [^OWLObjectAllValuesFrom a options]
  (list
   (unnamed-entity
    :object-only options)
   (form (.getProperty a) options)
   (form (.getFiller a) options)))

(defmethod form OWLObjectComplementOf
  [^OWLObjectComplementOf c options]
  (list
   (unnamed-entity
    :object-not options)
   (form (.getOperand c) options)))

(defmethod form OWLObjectExactCardinality
  [^OWLObjectExactCardinality c options]
  (list
   (unnamed-entity
    :object-exactly options)
   (.getCardinality c)
   (form (.getProperty c) options)
        (form (.getFiller c) options)))

(defmethod form OWLObjectMaxCardinality
  [^OWLObjectMaxCardinality c options]
  (list
   (unnamed-entity :object-at-most
                   options)
   (.getCardinality c)
        (form (.getProperty c) options)
        (form (.getFiller c) options)))

(defmethod form OWLObjectMinCardinality
  [^OWLObjectMinCardinality c options]
  (list (unnamed-entity :object-at-least options)
        (.getCardinality c)
        (form (.getProperty c) options)
        (form (.getFiller c) options)))

(defn- list**
  "Operates like list if the list or list* depending on whether the last
element is a list."
  [& args]
  (if (seq? (last args))
    (apply list* args)
    (apply list args)))

(defmethod form OWLAnnotation
  [^OWLAnnotation a options]
  (let [v (.getValue a)]
    (cond
     (.. a getProperty isLabel)
     (list (unnamed-entity :label options)
           (form v options))
     (.. a getProperty isComment)
     (list (unnamed-entity :comment options)
           (form v options))
     :default
     (list
      (unnamed-entity :annotation options)
      (form (.getProperty a) options)
      (form v options)))))

(defmethod form OWLAnnotationProperty [p options]
  (entity-or-iri p options))

;; this can be improved somewhat -- not converting classes into something
;; readable.
(defmethod form OWLAnnotationValue
  [^Object v options]
  (list
   (str v)))

(def
  ^{:private true}
  owldatatypes-inverted
  (into {}
        (for [[k
               ^OWL2Datatype v] owl/owl2datatypes]
          [(.getDatatype v (owl/owl-data-factory)) k])))

(defmethod form OWLLiteral
  [^OWLLiteral l options]
  (list*
   (unnamed-entity :literal options)
   (.getLiteral l)
   (if (.hasLang l)
     [:lang (.getLang l)]
     [:type
      (form (.getDatatype l) options)])))

;; so, in many cases, fillers can be an Datatype, which is probably going
;; to render as a keyword. Alternatively, it might be a DataRange which is
;; going to render as one or more span elements. The former needs to be
;; include directly, the latter needs not

(defmethod form OWLDataSomeValuesFrom
  [^OWLDataSomeValuesFrom d options]
  (list
   (unnamed-entity :data-some options)
   (form (.getProperty d) options)
   (form (.getFiller d) options)))

(defmethod form OWLDataAllValuesFrom
  [^OWLDataAllValuesFrom a options]
  (list
   (unnamed-entity :data-only options)
        (form (.getProperty a) options)
        (form (.getFiller a) options)))

(defmethod form OWLDataComplementOf
  [^OWLDataComplementOf c options]
  (list
   (unnamed-entity :data-not options)
   (form (.getDataRange c) options)))

(defmethod form OWLDataUnionOf
  [^OWLDataUnionOf c options]
  (list*
   (unnamed-entity :data-or options)
   (form (.getOperands c) options)))

(defmethod form OWLDataIntersectionOf
  [^OWLDataIntersectionOf c options]
  (list*
   (unnamed-entity :data-and options)
   (form (.getOperands c) options)))


(defmethod form OWLDataExactCardinality
  [^OWLDataExactCardinality c options]
  (list
   (unnamed-entity :data-exactly options)
   (.getCardinality c)
   (form (.getProperty c) options)
   (form (.getFiller c) options)))

(defmethod form OWLDataMaxCardinality
  [^OWLDataMaxCardinality c options]
  (list
   (unnamed-entity :data-at-most options)
   (.getCardinality c)
        (form (.getProperty c) options)
        (form (.getFiller c) options)))

(defmethod form OWLDataMinCardinality
  [^OWLDataMinCardinality c options]
  (list
   (unnamed-entity :data-at-least options)
   (.getCardinality c)
        (form (.getProperty c) options)
        (form (.getFiller c) options)))

(defmethod form OWLDataOneOf
  [^OWLDataOneOf c options]
  (list*
   (unnamed-entity :data-oneof options)
   (form (.getValues c) options)))

(defn- numeric-literal
  "Returns a number from one of the numerous typed wrappers."
  [^OWLLiteral l]
  (cond
   (.isInteger l)
   (.parseInteger l)
   (.isFloat l)
   (.parseFloat l)
   (.isDouble l)
   (.parseDouble l)
   :default
   (throw (IllegalArgumentException. "Non numeric literal passed to numeric-literal"))))

(defn- numeric-facet [d options]
  (get
       {OWLFacet/MAX_EXCLUSIVE
        (unnamed-entity :< options)
        OWLFacet/MAX_INCLUSIVE
        (unnamed-entity :<= options)
        OWLFacet/MIN_EXCLUSIVE
        (unnamed-entity :> options)
        OWLFacet/MIN_INCLUSIVE
        (unnamed-entity :>= options)
        }
       d))

(defmethod form OWLDatatypeRestriction
  [^OWLDatatypeRestriction d options]
  (let [dt (.getDatatype d)]
    (cond
     (or
      (.isDouble dt)
      (.isFloat dt)
      (.isInteger dt))
     (case (count (.getFacetRestrictions d))
       ;; we have a greater than or less than option
       1
       (let [^OWLFacetRestriction fr (first (.getFacetRestrictions d))]
         (list (unnamed-entity :span options)
               (numeric-facet (.getFacet fr) options)
               (numeric-literal (.getFacetValue fr))))
       ;; we have a greater than and less than option
       2
       (let
           ;; get the restrictions and ensure that are in the right order.
           [frs (sort (.getFacetRestrictions d))
            ^OWLFacetRestriction fr1 (nth frs 0)
            ^OWLFacetRestriction fr2 (nth frs 1)]
         (cond
          (and
           (=
            OWLFacet/MIN_INCLUSIVE
            (.getFacet fr1))
           (=
            OWLFacet/MAX_INCLUSIVE
            (.getFacet fr2)))
          (list (unnamed-entity :span options)
                (unnamed-entity :>=< options)
                (numeric-literal
                 (.getFacetValue fr1))
                (numeric-literal
                 (.getFacetValue fr2)))
          (and
           (=
            OWLFacet/MIN_EXCLUSIVE
            (.getFacet fr1))
           (=
            OWLFacet/MAX_EXCLUSIVE
            (.getFacet fr2)))
          (list (unnamed-entity :span options)
                (unnamed-entity :>< options)
                (numeric-literal
                 (.getFacetValue fr1))
                (numeric-literal
                 (.getFacetValue fr2)))
          :default
          (throw
           (Exception. "Have datatype with strange arrangement of facets."))))
       (throw
        (Exception.
         "Trying to render data type restriction with strange number of facets.")))
     :default
     (throw (Exception. "Can't render non-numeric datatype")))))

(defmethod form OWLFacetRestriction
  [^OWLFacetRestriction d options]
  (list (form (.getFacet d) options) (form (.getFacetValue d) options)))

(defmethod form org.semanticweb.owlapi.model.OWLDatatype [d options]
  (if-let [x (get owldatatypes-inverted d)]
    ;; it's a builtin, so reverse lookup keyword
    x
    (entity-or-iri d options)))

(defmethod form org.semanticweb.owlapi.model.OWLObjectHasValue
  [^OWLObjectHasValue p options]
  (list (unnamed-entity :object-has-value options)
        (form (.getProperty p) options)
        (form (.getValue p) options)))

(defmethod form org.semanticweb.owlapi.model.OWLObjectHasSelf
  [^OWLObjectHasSelf s options]
  (list
   (unnamed-entity :object-has-self options)
   (form (.getProperty s) options)))

(defmethod form org.semanticweb.owlapi.model.OWLDataHasValue
  [^OWLDataHasValue p options]
  (list (unnamed-entity :data-has-value options)
        (form (.getProperty p) options)
        (form (.getValue p) options)))

(defmethod form OWLObjectInverseOf
  [^OWLObjectInverseOf p options]
  (list
   (unnamed-entity :object-inverse options)
   (form (.getInverse p) options)))

(defmethod form org.semanticweb.owlapi.model.IRI [e options]
  (list (unnamed-entity :iri options)
        (str e)))

(defmethod form String [e options]
  e)

;; obviously this is error trap!
(defmethod form :default [e options]
  (throw
   (IllegalArgumentException.
    (str "Don't know how to render this form: " (class e)))))
