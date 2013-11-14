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
    (ensure-class o domain))))

(defbdontfn add-data-range
  {:doc "Adds a range to a data property."
   :arglists '([property & ranges] [ontology property & ranges])}
  [o property range]
  (add-axiom
   (.getOWLDataPropertyRangeAxiom
    (owl-data-factory)
    (ensure-data-property o property)
    (ensure-data-range o range))))

(defbdontfn add-data-superproperty
  "Adds a super property to a data property."
  [o property super]
  (add-axiom o
             (.getOWLSubDataPropertyOfAxiom
              (owl-data-factory)
              (ensure-data-property o property)
              (ensure-data-property o super))))
(def
  ^{:private true}
  datacharfuncs
  {
   :functional #(.getOWLFunctionalDataPropertyAxiom %1 %2)
   })

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
      (ensure-data-property o equivalent))))

(def ^{:private true}
  datatype-property-handlers
  {:annotation add-annotation,
   :domain add-data-domain,
   :range add-data-range,
   :subproperty add-data-superproperty
   :characteristic add-data-characteristics
   :equivalent add-data-equivalent
   :comment add-comment
   :label add-label})

(defdontfn datatype-property-explicit
  "Define a new datatype property with an explicit map"
  [o name frames]
  (let [o (or (first (get frames :ontology)) o)
        property (ensure-data-property o name)]
    (.addAxiom (owl-ontology-manager)
               o
               (.getOWLDeclarationAxiom
                (owl-data-factory)
                property))
    (add-a-name-annotation o property name)
    (doseq [[k f] datatype-property-handlers]
      (f o property (get frames k)))
    property))

(defdontfn datatype-property
  "Define a new datatype property"
  [o name & frames]
  (let [keys
        (list* :ontology (keys datatype-property-handlers))]
    (datatype-property-explicit
     o name
     (util/check-keys
      (util/hashify-at
       keys frames)
      keys))))

(defmacro defdproperty
  "Defines a new datatype property and interns as a var."
  [dataname & frames]
  `(let [namestring# (name '~dataname)
         datatype# (tawny.owl/datatype-property namestring#
                                                ~@frames)]
     (intern-owl ~dataname datatype#)))


(defmontfn literal
  "Returns a OWL2 literal.

`literal' is the value of the literal and must be a string or a number. Anything
else must by coerced into a string manually. Options can also be specified,
with :lang definining the language where `literal' is a string, and :type
which is an OWLDatatype object.
"
  [o literal & {:keys [lang type]}]
  (cond
   lang
   (.getOWLLiteral (owl-data-factory) literal lang)
   type
   (.getOWLLiteral (owl-data-factory)
                   literal
                   (ensure-datatype o type))
   :default
   (.getOWLLiteral (owl-data-factory) literal)))


(defbdontfn add-datatype-equivalent
  "Adds a datatype equivalent axiom"
  [o datatype equivalent]
  (add-axiom
   o (.getOWLDatatypeDefinitionAxiom
      (owl-data-factory) datatype
      (ensure-data-range o equivalent))))

(def ^{:private true}
  datatype-handlers
  {:annotation add-annotation
   :comment add-comment
   :label add-label
   :equivalent add-datatype-equivalent})

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
    (doseq [[k f] datatype-handlers]
      (f o datatype (get frames k)))
    datatype))

(defdontfn datatype
  "Defines a new datatype."
  [o name & frames]
  (let [keys
        (list* :ontology
               (keys datatype-handlers))]
    (datatype-explicit
     o name
     (util/check-keys
      (util/hashify-at keys frames)
      keys))))

(defmacro defdatatype
  "Defines a new datatype and interns it as a var."
  [dataname & frames]
  `(let [namestring# (name '~dataname)
         datatype# (tawny.owl/datatype namestring#
                                       ~@frames)]
     (intern-owl ~dataname datatype#)))

(defmontfn data-and
  "Returns the intersection of two data ranges."
  [_ & types]
  (.getOWLDataIntersectionOf
   (owl-data-factory)
   (into #{} types)))

(.addMethod owl-and :data data-and)

(defmontfn data-or
  "Returns the union of two data ranges."
  [o & types]
  (.getOWLDataUnionOf
   (owl-data-factory)
   (into #{} (map (partial ensure-data-range o) types))))

(.addMethod owl-or :data data-or)

(defmontfn data-not
  "Returns the complement of two data ranges."
  [o type]
  (.getOWLDataComplementOf
   (owl-data-factory)
   (ensure-data-range o type)))

(.addMethod owl-not :data data-not)

(defbmontfn data-some
  "Returns a data some values from restriction."
  [o property data-range]
  (.getOWLDataSomeValuesFrom
   (owl-data-factory)
   (ensure-data-property o property)
   (ensure-data-range o data-range)))

(.addMethod owl-some :data data-some)

(defbmontfn data-only
  "Returns a data all values from restriction."
  [o property datatype]
  (.getOWLDataAllValuesFrom
   (owl-data-factory)
   (ensure-data-property o property)
   (ensure-data-range o datatype)))

(.addMethod only :data data-only)

(defmontfn data-oneof
  "Returns a data one of restriction."
  [o & literal]
  (.getOWLDataOneOf
   (owl-data-factory)
   (into #{}
         literal)))

(.addMethod oneof :literal data-oneof)

(defmontfn data-has-value
  "Returns a data has value restriction."
  [o property literal]
  (.getOWLDataHasValue (owl-data-factory)
   (ensure-data-property o property)
   (if (instance? OWLLiteral literal)
     literal
     (tawny.owl/literal literal))))

(.addMethod has-value :data data-has-value)

(defmontfn data-exactly
  "Returns a data exact cardinality restriction."
  [o number property]
  (.getOWLDataExactCardinality
   (owl-data-factory)
   number (ensure-data-property o property)))

(.addMethod exactly :data data-exactly)

(defmontfn data-at-most
  "Returns a data max cardinality restriction."
  [o number property]
  (.getOWLDataMaxCardinality
   (owl-data-factory) number
   (ensure-data-property o property)))

(.addMethod at-most :data data-at-most)

(defmontfn data-at-least
  "Returns a data min cardinality restriction."
  [o number property]
  (.getOWLDataMinCardinality
   (owl-data-factory) number
   (ensure-data-property o property)))

(.addMethod at-least :data data-at-least)

(defn owl-min
  "Returns a data min exclusion restriction."
  [from]
  (.getOWLDatatypeMinExclusiveRestriction
   (owl-data-factory) from))

(defn owl-max
  "Returns a data max exclusion restriction."
  [to]
  (.getOWLDatatypeMaxExclusiveRestriction
   (owl-data-factory) to))

(defn min-max
  "Returns a min-max exclusive restriction."
  [from to]
  (.getOWLDatatypeMinMaxExclusiveRestriction
   (owl-data-factory) from to))

(defn min-inc
  "Returns a min inclusive restriction."
  [from]
  (.getOWLDatatypeMinInclusiveRestriction
   (owl-data-factory) from))

(defn max-inc
  "Returns a max inclusive restriction."
  [to]
  (.getOWLDatatypeMaxInclusiveRestriction
   (owl-data-factory) to))

(defn min-max-inc
  "Returns a min max inclusive restriction."
  [from to]
  (.getOWLDatatypeMinMaxInclusiveRestriction
   (owl-data-factory) from to))

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
  [o property from to]
  (.getOWLDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property o property) from to))

(.addMethod get-fact :data data-get-fact)

(defmontfn data-get-fact-not
  "Returns a data negative fact."
  [o property from to]
  (.getOWLNegativeDataPropertyAssertionAxiom
   (owl-data-factory)
   (ensure-data-property o property) from to))

(.addMethod get-fact-not :data data-get-fact-not)
