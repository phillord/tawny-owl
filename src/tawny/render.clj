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
    :>= '>=}})

(def named-entity-map
  {OWLClass [:class 'defclass 'owl-class]
   OWLObjectProperty [:oproperty 'defoproperty 'object-property]
   OWLNamedIndividual [:individual 'defindividual 'individual]
   OWLDataProperty [:dproperty 'defdproperty 'datatype-property]
   OWLAnnotationProperty [:aproperty 'defaproperty 'annotation-property]})

(defn- unnamed-entity [entity-keyword options]
  (get
   (get options ::unnamed)
   entity-keyword))

(defn named-entity [type entity options]
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

(defn named-entity-as-string
  "Return a string identifier for an entity"
  [^OWLNamedObject entity]
  (-> entity
      (.getIRI)
      (.toURI)
      (.toString)))

(defn ^java.util.Set ontologies
  "Fetch all known ontologies."
  [options]
  (or (get options :ontologies)
      (and (get options :manager)
           (.getOntologies
            ^org.semanticweb.owlapi.model.OWLOntologyManager (get options :manager)))
      (.getOntologies
       (owl/owl-ontology-manager))))

(defn setmap
  "Apply f to list c, union the results."
  [f c]
  (apply clojure.set/union (map f c)))

(declare form)

(defmulti ^{:private true} as-form-int
  "Render one of the main OWLEntities as one of the main functions
in tawny." (fn [c options]
             (class c)))

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
            characteristic)))))

(defrecord ^{:private true} FactList
    [type facts])

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
        (filter
         (complement nil?)
         (list
          (let [fs
                (into {} (setmap #(.getObjectPropertyValues p %) ont))]
            (when (seq fs)
              (FactList. :object-fact fs)))
          (let [fs
                (into {} (setmap #(.getDataPropertyValues p %) ont))]
            (when (seq fs)
              (FactList. :data-fact fs)))
          (let [fs
                (into {} (setmap #(.getNegativeObjectPropertyValues p %) ont))]
            (when (seq fs)
              (FactList. :object-fact-not fs)))
          (let [fs
                (into {} (setmap #(.getNegativeDataPropertyValues p %) ont))]
            (when (seq fs)
              (FactList. :data-fact-not fs)))))
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
        ;; so, we use a PersistentVector to distinguish here between postive
        ;; and negative
        (form facts options))))))

(defmethod as-form-int OWLDataProperty
  [^OWLDataProperty p options]
  (let [ont (ontologies options)
        domain (.getDomains p ont)
        range (.getRanges p ont)
        superprop (.getSuperProperties p ont)
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
            characteristic)))))

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
  [entity & options]
  (let [;;a (println "options" options)
        options (apply hash-map options)
        ;;a (println "options now " options)
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

(defmulti ^{:private true} form
  "Render any OWLEntity or collections containing these entities as Clojure
forms."
  (fn [c options]
    (class c)))

;; how to get from {:a {1 2}} {:b {3 4}}
;; to [:a 1][:a 2]
;; or support (fact I1 I2)?

(defmethod form clojure.lang.ISeq [s options]
  (doall (map #(form % options) s)))

(defmethod form Set [s options]
  (map #(form % options) s))

(defmethod form java.util.Map [m options]
  (for
   [[k v] m]
   `(~(form k options) ~(form v options))))

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


(defmethod form FactList [s options]
  (let [type-sym (unnamed-entity (:type s) options)]
    (mapcat
     (fn create-fact-forms
       [property]
       (interleave
         (repeat type-sym)
         (repeat (form property options))
         (map #(form % options)
              (seq (get (:facts s) property)))))
     ;; each of the properties
     (keys (:facts s)))))


(defmethod form OWLClass [c options]
  (entity-or-iri c options))

(defmethod form OWLProperty [p options]
  (entity-or-iri p options))

(defmethod form OWLIndividual [i options]
  (entity-or-iri i options))

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
     (do
       ;;(println "v is" v)
       (list
        (unnamed-entity :annotation options)
        (form (.getProperty a) options)
        (form v options))))))

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
  (list**
   (unnamed-entity :data-some options)
   (form (.getProperty d) options)
   (form (.getFiller d) options)))

(defmethod form OWLDataAllValuesFrom
  [^OWLDataAllValuesFrom a options]
  (list**
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
     (for [^OWLFacetRestriction fr (.getFacetRestrictions d)]
       (list (unnamed-entity :span options)
             (numeric-facet (.getFacet fr) options)
             (numeric-literal (.getFacetValue fr))))
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
    (str "Don't know how to render this form:" e))))
