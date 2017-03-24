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

(defnb2 add-data-domain
  "Adds a domain to a data property."
  [o property domain]
  (add-axiom o
             (.getOWLDataPropertyDomainAxiom
              (owl-data-factory)
              (ensure-data-property property)
              (ensure-class domain)
              (p/as-annotations domain))))

(defnb2 add-data-range
  "Adds a range to a data property."
  [o property range]
  (add-axiom
   (.getOWLDataPropertyRangeAxiom
    (owl-data-factory)
    (ensure-data-property property)
    (ensure-data-range range)
    (p/as-annotations range))))

(defnb2 add-data-subproperty
  "Adds a sub property to a data property."
  [o property sub]
  (add-axiom
   o
   (.getOWLSubDataPropertyOfAxiom
    (owl-data-factory)
    (ensure-data-property sub)
    (ensure-data-property property)
    (p/as-annotations sub))))

(defnb2 add-data-superproperty
  "Adds a super property to a data property."
  [o property super]
  (add-axiom o
             (.getOWLSubDataPropertyOfAxiom
              (owl-data-factory)
              (ensure-data-property property)
              (ensure-data-property super)
              (p/as-annotations super))))

(def
  ^{:deprecated "1.1"
    :doc "The same as add-data-superproperty but deprecated.
This is to deprecated the :superproperty frame"}
  deprecated-add-data-superproperty
  add-data-superproperty)


(def
  ^{:private true}
  datacharfuncs
  {
   :functional
   (fn [^OWLDataFactory df
        ^OWLDataProperty dp]
     (.getOWLFunctionalDataPropertyAxiom
      df
      (p/as-entity dp)
      (p/as-annotations dp)))})

(defnb2 add-data-characteristics
  "Add a list of characteristics to the property."
  [o property characteristic]
  (when-not (get datacharfuncs characteristic)
    (throw (IllegalArgumentException.
            "Characteristic is not recognised:" characteristic)))
  (add-axiom o
             ((get datacharfuncs characteristic)
              (owl-data-factory) (ensure-data-property property))))

(defnb2 add-data-equivalent
  "Adds a equivalent data properties axiom."
  [o property equivalent]
  (add-axiom
   o (.getOWLEquivalentDataPropertiesAxiom
      (owl-data-factory)
      (ensure-data-property property)
      (ensure-data-property equivalent)
      (p/as-annotations equivalent))))

(defn equivalent-data-properties
  [o properties]
  (let [properties
        (doall
         (map ensure-data-property properties))]
    (add-axiom
     o (.getOWLEquivalentDataPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (union-annotations properties)))))

(defnb2 add-data-disjoint
  {:doc "Adds a disjoint data property axiom to the ontology"}
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointDataPropertiesAxiom
    (owl-data-factory)
    (hash-set
     (ensure-data-property name)
     (ensure-data-property disjoint))
    (p/as-annotations disjoint))))

(defn disjoint-data-properties
  [o properties]
  (let [properties
        (doall
         (map ensure-data-property properties))]
    (add-axiom
     o (.getOWLDisjointDataPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (p/as-annotations properties)))))

(def ^{:private true} datatype-property-handlers
  {
   :annotation #'add-annotation,
   :domain #'add-data-domain,
   :range #'add-data-range,
   :subproperty #'deprecated-add-data-superproperty
   :sub #'add-data-subproperty
   :super #'add-data-superproperty
   :characteristic #'add-data-characteristics
   :disjoint #'add-data-disjoint
   :equivalent #'add-data-equivalent
   :comment #'add-comment
   :label #'add-label
   })

(defno datatype-property-explicit
  "Define a new datatype property with an explicit map"
  [o name frames]
  (let [property
        (ensure-data-property
         (if (string? name)
           (iri-for-name o name)
           name))]
    (.addAxiom (owl-ontology-manager)
               o
               (.getOWLDeclarationAxiom
                (owl-data-factory)
                property))
    (add-a-name-annotation o property name)
    (doseq [[k f] datatype-property-handlers
            :when (get frames k)]
      (f o property (get frames k)))
    property))

(defno datatype-property
  "Define a new datatype property"
  [o name & frames]
  (let [keys
        (keys datatype-property-handlers)
        frames
        (util/check-keys
         (util/hashify-at
          keys frames)
         keys)]
    (datatype-property-explicit
     o name frames)))

(defentity defdproperty
  "Defines a new datatype property"
  'tawny.owl/datatype-property)

(defn literal
  "Returns a OWL2 literal.

`literal' is the value of the literal and must be a string or a number. Anything
else must by coerced into a string manually. Options can also be specified,
with :lang definining the language where `literal' is a string, and :type
which is an OWLDatatype object.
"
  [literal & {:keys [lang type]}]
  (cond
   ;; null operation
   (t/literal? literal)
   literal
   lang
   (.getOWLLiteral (owl-data-factory) ^String literal ^String lang)
   type
   (.getOWLLiteral (owl-data-factory)
                   ^String literal
                   (ensure-datatype type))
   :default
   (util/with-types
     [literal [String Long Double Boolean]]
     (.getOWLLiteral (owl-data-factory) literal))))

(defnb2 add-datatype-equivalent
  "Adds a datatype equivalent axiom"
  [o datatype equivalent]
  (add-axiom
   o (.getOWLDatatypeDefinitionAxiom
      (owl-data-factory) datatype
      (ensure-data-range equivalent)
      (p/as-annotations equivalent))))

(def ^{:private true} datatype-handlers
  {
   :annotation #'add-annotation
   :comment #'add-comment
   :label #'add-label
   :equivalent #'add-datatype-equivalent
   })

(defno datatype-explicit
  "Defines a new datatype."
  [o name frames]
  (let [o
        (or (first (get frames :ontology))
            o)
        datatype
        (.getOWLDatatype
         (owl-data-factory)
         (iri-for-name o name))]
    (add-axiom o
     (.getOWLDeclarationAxiom (owl-data-factory) datatype))
    (add-a-name-annotation o datatype name)
    (doseq [[k f] datatype-handlers
            :when (get frames k)]
      (f o datatype (get frames k)))
    datatype))

(defno datatype
  "Defines a new datatype."
  [o name & frames]
  (let [keys
        (list* :ontology
               (keys datatype-handlers))
        frames
        (util/check-keys
         (util/hashify-at keys frames)
         keys)
        o (or (first (get frames :ontology))
              o)]
    (datatype-explicit
     o name frames)))

(defentity defdatatype
  "Defines a new datatype"
  'tawny.owl/datatype)

(defn data-and
  "Returns the intersection of two data ranges."
  [& types]
  (.getOWLDataIntersectionOf
   (owl-data-factory)
   ^java.util.Set
   (set
    (map ensure-data-range
         types))))

(util/defmethodf owl-and ::data data-and)

(defn data-or
  "Returns the union of two data ranges."
  [& types]
  (.getOWLDataUnionOf
   (owl-data-factory)
   ^java.util.Set
   (set (map ensure-data-range types))))

(util/defmethodf owl-or ::data data-or)

(defn data-not
  "Returns the complement of two data ranges."
  [type]
  (.getOWLDataComplementOf
   (owl-data-factory)
   (ensure-data-range type)))

(util/defmethodf owl-not-one ::data data-not)

(defnb data-some
  "Returns a data some values from restriction."
  [property data-range]
  (.getOWLDataSomeValuesFrom
   (owl-data-factory)
   (ensure-data-property property)
   (ensure-data-range data-range)))

(util/defmethodf owl-some ::data data-some)

(def data-only
  "Returns a data all values from restriction."
  (broadcast
   (fn data-only
     [property datatype]
     (.getOWLDataAllValuesFrom
      (owl-data-factory)
      (ensure-data-property property)
      (ensure-data-range datatype)))))

(util/defmethodf only ::data data-only)

(defn data-oneof
  "Returns a data one of restriction."
  [& literals]
  (.getOWLDataOneOf
   (owl-data-factory)
   ^java.util.Set
   (set (map literal literals))))

(util/defmethodf oneof ::literal data-oneof)

(defn data-has-value
  "Returns a data has value restriction."
  [property literal]
  (.getOWLDataHasValue (owl-data-factory)
   (ensure-data-property property)
   (if (t/literal? literal)
     literal
     (tawny.owl/literal literal))))

(util/defmethodf has-value ::data data-has-value)

(def data-exactly
  "Returns a data exact cardinality restriction."
  (fn data-exactly
    [number property]
    (.getOWLDataExactCardinality
     (owl-data-factory)
     number (ensure-data-property property))))

(util/defmethodf exactly ::data data-exactly)

(defn data-at-most
  "Returns a data max cardinality restriction."
  [number property]
  (.getOWLDataMaxCardinality
   (owl-data-factory) number
   (ensure-data-property property)))

(util/defmethodf at-most ::data data-at-most)

(defn data-at-least
  "Returns a data min cardinality restriction."
  [number property]
  (.getOWLDataMinCardinality
   (owl-data-factory) number
   (ensure-data-property property)))

(util/defmethodf at-least ::data data-at-least)

(defn owl-min
  "Returns a data min exclusion restriction."
  [from]
  (util/with-types [from [Long Double]]
    (.getOWLDatatypeMinExclusiveRestriction
     (owl-data-factory) from)))

(defn owl-max
  "Returns a data max exclusion restriction."
  [to]
  (util/with-types [to [Long Double]]
    (.getOWLDatatypeMaxExclusiveRestriction
     (owl-data-factory) to)))

(defn min-max
  "Returns a min-max exclusive restriction."
  [from to]
  (cond
   (instance? Long from)
   (.getOWLDatatypeMinMaxExclusiveRestriction
    (owl-data-factory) ^Long from ^Long to)
   (instance? Double from)
   (.getOWLDatatypeMinMaxExclusiveRestriction
    (owl-data-factory) ^Double from ^Double to)))

(defn min-inc
  "Returns a min inclusive restriction."
  [from]
  (util/with-types [from [Long Double]]
    (.getOWLDatatypeMinInclusiveRestriction
     (owl-data-factory) from)))

(defn max-inc
  "Returns a max inclusive restriction."
  [to]
  (util/with-types [to [Long Double]]
    (.getOWLDatatypeMaxInclusiveRestriction
     (owl-data-factory) to)))

(defn min-max-inc
  "Returns a min max inclusive restriction."
  [from to]
  (cond
   (instance? Long from)
   (.getOWLDatatypeMinMaxInclusiveRestriction
    (owl-data-factory) ^Long from ^Long to)
   (instance? Double from)
   (.getOWLDatatypeMinMaxInclusiveRestriction
    (owl-data-factory) ^Double from ^Double to)))

(defmacro span
  "Returns a numeric datatype restriction, identified by symbol.
For example, (span < 10) returns a max exclusive restriction."
  [comparitor & args]
  (cond
   (= comparitor '<)
   `(apply owl-max '~args)
   (= comparitor '>)
   `(apply owl-min '~args)
   (= comparitor '><)
   `(apply min-max '~args)
   (= comparitor '<=)
   `(apply max-inc '~args)
   (= comparitor '>=)
   `(apply min-inc '~args)
   (= comparitor '>=<)
   `(apply min-max-inc '~args)
   :default
   (throw (IllegalArgumentException. (str "Unknown comparitor" comparitor)))))

(defn data-get-fact
  "Returns a data fact."
  [property ^OWLIndividual from to annotations]
  (.getOWLDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property property) from
   (literal to) annotations))

(util/defmethodf get-fact ::data data-get-fact)

(defn data-get-fact-not
  "Returns a data negative fact."
  [property from to annotations]
  (.getOWLNegativeDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property property) from to annotations))

(util/defmethodf get-fact-not ::data data-get-fact-not)
