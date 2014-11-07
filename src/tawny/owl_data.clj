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

(defbdontfn add-data-domain
  {:doc "Adds a domain to a data property."
   :arglists '([property & domains] [o property & domains])}
  [o property domain]
  (add-axiom o
   (.getOWLDataPropertyDomainAxiom
    (owl-data-factory)
    (ensure-data-property o property)
    (ensure-class o domain)
    (as-annotations domain))))

(defbdontfn add-data-range
  {:doc "Adds a range to a data property."
   :arglists '([property & ranges] [ontology property & ranges])}
  [o property range]
  (add-axiom
   (.getOWLDataPropertyRangeAxiom
    (owl-data-factory)
    (ensure-data-property o property)
    (ensure-data-range o range)
    (as-annotations range))))

(defbdontfn add-data-subproperty
  "Adds a sub property to a data property."
  [o property sub]
  (add-axiom
   o
   (.getOWLSubDataPropertyOfAxiom
    (owl-data-factory)
    (ensure-data-property o sub)
    (ensure-data-property o property)
    (as-annotations sub))))

(defbdontfn add-data-superproperty
  "Adds a super property to a data property."
  [o property super]
  (add-axiom o
             (.getOWLSubDataPropertyOfAxiom
              (owl-data-factory)
              (ensure-data-property o property)
              (ensure-data-property o super)
              (as-annotations super))))

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
      (as-entity dp)
      (as-annotations dp)))})

(defbdontfn add-data-characteristics
  "Add a list of characteristics to the property."
  [o property characteristic]
  (when-not (get datacharfuncs characteristic)
    (throw (IllegalArgumentException.
            "Characteristic is not recognised:" characteristic)))
  (add-axiom o
             ((get datacharfuncs characteristic)
              (owl-data-factory) (ensure-data-property o property))))

(defbdontfn add-data-equivalent
  "Adds a equivalent data properties axiom."
  [o property equivalent]
  (add-axiom
   o (.getOWLEquivalentDataPropertiesAxiom
      (owl-data-factory)
      (ensure-data-property o property)
      (ensure-data-property o equivalent)
      (as-annotations equivalent))))

(defdontfn equivalent-data-properties
  [o properties]
  (let [properties
        (doall
         (map (partial ensure-data-property o) properties))]
    (add-axiom
     o (.getOWLEquivalentDataPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (union-annotations properties)))))

(defbdontfn add-data-disjoint
  {:doc "Adds a disjoint data property axiom to the ontology"}
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointDataPropertiesAxiom
    (owl-data-factory)
    (hash-set
     (ensure-data-property o name)
     (ensure-data-property o disjoint))
    (as-annotations disjoint))))

(defdontfn disjoint-data-properties
  [o properties]
  (let [properties
        (doall
         (map (partial ensure-data-property o) properties))]
    (add-axiom
     o (.getOWLDisjointDataPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (as-annotations properties)))))

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

(defdontfn datatype-property-explicit
  "Define a new datatype property with an explicit map"
  [o name frames]
  (let [property (ensure-data-property o name)]
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

(defdontfn datatype-property
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

(comment
 (defmacro defdproperty
   "Defines a new datatype property and interns as a var."
   [dataname & frames]
   `(let [namestring# (name '~dataname)
          datatype# (tawny.owl/datatype-property namestring#
                                                 ~@frames)]
      (intern-owl ~dataname datatype#))))


(defmontfn literal
  "Returns a OWL2 literal.

`literal' is the value of the literal and must be a string or a number. Anything
else must by coerced into a string manually. Options can also be specified,
with :lang definining the language where `literal' is a string, and :type
which is an OWLDatatype object.
"
  [o literal & {:keys [lang type]}]
  (cond
   ;; null operation
   (instance? OWLLiteral literal)
   literal
   lang
   (.getOWLLiteral (owl-data-factory) ^String literal ^String lang)
   type
   (.getOWLLiteral (owl-data-factory)
                   ^String literal
                   (ensure-datatype o type))
   :default
   (util/with-types
     [literal [String Long Double Boolean]]
     (.getOWLLiteral (owl-data-factory) literal))))

(defbdontfn add-datatype-equivalent
  "Adds a datatype equivalent axiom"
  [o datatype equivalent]
  (add-axiom
   o (.getOWLDatatypeDefinitionAxiom
      (owl-data-factory) datatype
      (ensure-data-range o equivalent)
      (as-annotations equivalent))))

(def ^{:private true} datatype-handlers
  {
   :annotation #'add-annotation
   :comment #'add-comment
   :label #'add-label
   :equivalent #'add-datatype-equivalent
   })

(defdontfn datatype-explicit
  "Defines a new datatype."
  [o name frames]
  (let [o
        (or (first (get frames :ontology))
            o)
        datatype
        (.getOWLDatatype
         (owl-data-factory)
         (iri-for-name name))]
    (add-axiom o
     (.getOWLDeclarationAxiom (owl-data-factory) datatype))
    (add-a-name-annotation o datatype name)
    (doseq [[k f] datatype-handlers
            :when (get frames k)]
      (f o datatype (get frames k)))
    datatype))

(defdontfn datatype
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

(comment
  (defmacro defdatatype
    "Defines a new datatype and interns it as a var."
    [dataname & frames]
    `(let [namestring# (name '~dataname)
           datatype# (tawny.owl/datatype namestring#
                                         ~@frames)]
       (intern-owl ~dataname datatype#))))

(defmontfn data-and
  "Returns the intersection of two data ranges."
  [o & types]
  (.getOWLDataIntersectionOf
   (owl-data-factory)
   ^java.util.Set
   (set
    (map (partial ensure-data-range o)
         types))))

(defmethod owl-and ::data [& rest]
  (apply data-and rest))

(defmontfn data-or
  "Returns the union of two data ranges."
  [o & types]
  (.getOWLDataUnionOf
   (owl-data-factory)
   ^java.util.Set
   (set (map (partial ensure-data-range o) types))))

(defmethod owl-or ::data [& rest]
  (apply data-or rest))

(defmontfn data-not
  "Returns the complement of two data ranges."
  [o type]
  (.getOWLDataComplementOf
   (owl-data-factory)
   (ensure-data-range o type)))

(defmethod owl-not-one ::data [& rest]
  (apply data-not rest))

(defbmontfn data-some
  "Returns a data some values from restriction."
  [o property data-range]
  (.getOWLDataSomeValuesFrom
   (owl-data-factory)
   (ensure-data-property o property)
   (ensure-data-range o data-range)))

(defmethod owl-some ::data [& rest]
  (apply data-some rest))

(defbmontfn data-only
  "Returns a data all values from restriction."
  [o property datatype]
  (.getOWLDataAllValuesFrom
   (owl-data-factory)
   (ensure-data-property o property)
   (ensure-data-range o datatype)))

(defmethod only ::data [& rest]
  (apply data-only rest))

(defmontfn data-oneof
  "Returns a data one of restriction."
  [o & literals]
  (.getOWLDataOneOf
   (owl-data-factory)
   ^java.util.Set
   (set (map (partial literal o) literals))))

(defmethod oneof ::literal [& rest]
  (apply data-oneof rest))

(defmontfn data-has-value
  "Returns a data has value restriction."
  [o property literal]
  (.getOWLDataHasValue (owl-data-factory)
   (ensure-data-property o property)
   (if (instance? OWLLiteral literal)
     literal
     (tawny.owl/literal literal))))

(defmethod has-value ::data [& rest]
  (apply data-has-value rest))

(defmontfn data-exactly
  "Returns a data exact cardinality restriction."
  [o number property]
  (.getOWLDataExactCardinality
   (owl-data-factory)
   number (ensure-data-property o property)))

(defmethod exactly ::data [& rest]
  (apply data-exactly rest))

(defmontfn data-at-most
  "Returns a data max cardinality restriction."
  [o number property]
  (.getOWLDataMaxCardinality
   (owl-data-factory) number
   (ensure-data-property o property)))

(defmethod at-most ::data [& rest]
  (apply data-at-most rest))

(defmontfn data-at-least
  "Returns a data min cardinality restriction."
  [o number property]
  (.getOWLDataMinCardinality
   (owl-data-factory) number
   (ensure-data-property o property)))

(defmethod at-least ::data [& rest]
  (apply data-at-least rest))

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

(defmontfn data-get-fact
  "Returns a data fact."
  [^OWLOntology o property ^OWLIndividual from to annotations]
  (.getOWLDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property o property) from
   (literal o to) annotations))

(defmethod get-fact ::data [& rest]
  (apply data-get-fact rest))

(defmontfn data-get-fact-not
  "Returns a data negative fact."
  [o property from to annotations]
  (.getOWLNegativeDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property o property) from to annotations))

(defmethod get-fact-not ::data [& rest]
  (apply data-get-fact-not rest))
