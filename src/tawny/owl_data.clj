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


;; data type properties
(defn- ensure-data-property [o property]
  "Ensures that 'property' is an data property,
converting it from a string or IRI if necessary."
  (cond
   (instance? OWLDataProperty property)
   property
   (instance? IRI property)
   (.getOWLDataProperty
    ontology-data-factory property)
   (instance? String property)
   (ensure-data-property o
    (iriforname o property))
   :default
   (throw (IllegalArgumentException.
           (format "Expecting an OWL data property: %s" property)))))

(defbdontfn add-data-domain
  {:doc "Adds a domain to a data property."
   :arglists '([property & domains] [o property & domains])}
  [o property domain]
  (add-axiom o
   (.getOWLDataPropertyDomainAxiom
    ontology-data-factory
    (ensure-data-property o property)
    (ensure-class o domain))))

(defbdontfn add-data-range
  {:doc "Adds a range to a data property."
   :arglists '([property & ranges] [ontology property & ranges])}
  [o property range]
  (add-axiom
   (.getOWLDataPropertyRangeAxiom
    ontology-data-factory
    (ensure-data-property o property)
    range)))

(defbdontfn add-data-superproperty
  [o property super]
  (add-axiom o
             (.getOWLSubDataPropertyOfAxiom
              ontology-data-factory
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
              ontology-data-factory (ensure-data-property o property))))



(def xsd:float
  (.getFloatOWLDatatype ontology-data-factory))

(def xsd:double
  (.getDoubleOWLDatatype ontology-data-factory))

(def xsd:integer
  (.getIntegerOWLDatatype ontology-data-factory))

(def rdf:plainliteral
  (.getRDFPlainLiteral ontology-data-factory))

;; TODO
;; need to create accessor methods for all the data ranges. Sadly, also need
;; to think what to do about or and and, which they also support. In the ideal
;; world I don't want to introduce new functions, although I have already had
;; to with add-data-range and so on. What a pain in the ass!
;; the ensure-class/property things can be extended to for an IRI to check if
;; it is already in the signature of any known ontology

(defdontfn datatypeproperty-explicit
  "Define a new datatype property with an explicit map"
  [o property map]
  (let [o (or (first (get map :ontology))
              o)
        dataproperty (ensure-data-property o property)]
    (.addAxiom owl-ontology-manager
               o
               (.getOWLDeclarationAxiom
                ontology-data-factory
                dataproperty))
    (add-annotation o dataproperty (:annotation map))
    (add-data-domain o dataproperty (:domain map))
    (add-data-range o dataproperty (:range map))
    (add-data-superproperty o dataproperty (:subproperty map))
    (add-data-characteristics o dataproperty (:characteristic map))
    (when-let [comment (:comment map)]
      (add-annotation o
                      dataproperty
                      (list (owlcomment (first comment)))))

    (when-let [labl (:label map)]
      (add-annotation o dataproperty
                      (list (label (first labl)))))

    (when (instance? String property)
      (add-a-simple-annotation
       o dataproperty (tawny-name property)))

    dataproperty))

(defdontfn datatypeproperty
  "Define a new datatype property"
  [o name & frames]
  (let [keys
        [:domain :range :annotation :characteristic
         :subproperty :equivalent :disjoint :ontology
         :label :comment]]
    (datatypeproperty-explicit
     o name
     (util/check-keys
      (util/hashify-at
       keys frames)
      keys))))

(defmacro defdproperty
  [dataname & frames]
  `(let [namestring# (name '~dataname)
         datatype# (tawny.owl/datatypeproperty namestring#
                                               ~@frames)]
     (def
       ~(vary-meta dataname
                   merge
                   {:owl true})
       datatype#)))


(defmontfn literal
  "Returns a OWL2 literal.

`literal' is the value of the literal and must be a string or a number. Anything
else must by coerced into a string manually. Options can also be specified,
with :lang definining the language where `literal' is a string, and :type
which is an OWLDatatype object.
"
  [_ literal & {:keys [lang type]}]
  (cond
   lang
   (.getOWLLiteral ontology-data-factory literal lang)
   type
   (.getOWLLiteral ontology-data-factory literal type)
   :default
   (.getOWLLiteral ontology-data-factory literal)))


(defdontfn datatype-explicit [o name frame]
  (let [o
        (or (first (get frame :ontology))
            o)
        datatype
        (.getOWLDatatype
         ontology-data-factory
         (iriforname name))]
    (add-axiom o
     (.getOWLDeclarationAxiom ontology-data-factory datatype))

    (add-annotation
     o datatype
     (concat
      (:annotation frame)
      (map label (:label frame))
      (map owlcomment (:comment frame))))
    
    (when (instance? String name)
      (add-a-simple-annotation
       o datatype (tawny-name name)))

    (doseq
        [n (:equivalent frame)]
      (add-axiom o
       (.getOWLDatatypeDefinitionAxiom
        ontology-data-factory datatype n)))
    datatype))

(defdontfn datatype [o name & frames]
  (datatype-explicit
   o name
   (util/check-keys
    (util/hashify frames)
    [:equivalent :annotation :label :comment])))


(defmacro defdatatype
  [dataname & frames]
  `(let [namestring# (name '~dataname)
         datatype# (tawny.owl/datatype namestring#
                                       ~@frames)]
     (def
       ~(vary-meta dataname
                   merge
                   {:owl true})
       datatype#)))

(defmontfn data-and
  [_ & types]
  (.getOWLDataIntersectionOf
   ontology-data-factory
   (into #{} types)))

(.addMethod owland :data data-and)

(defmontfn data-or
  [_ & types]
  (.getOWLDataUnionOf
   ontology-data-factory
   (into #{} types)))

(.addMethod owlor :data data-or)

(defmontfn data-not
  [_ type]
  (.getOWLDataComplementOf
   ontology-data-factory type))

(.addMethod owlnot :data data-not)

(defbmontfn data-some
  [o property datatype]
  (.getOWLDataSomeValuesFrom
   ontology-data-factory
   (ensure-data-property o property) datatype))

(.addMethod owlsome :data data-some)

(defbmontfn data-only
  [o property datatype]
  (.getOWLDataAllValuesFrom
   ontology-data-factory
   (ensure-data-property o property) datatype))


(.addMethod only :data data-only)

(defmontfn data-oneof [_ & data]
  (.getOWLDataOneOf
   ontology-data-factory
   (into #{} data)))

(.addMethod oneof :literal data-oneof)

(defmontfn data-hasvalue [o property literal]
  (.getOWLDataHasValue ontology-data-factory
   (ensure-data-property o property)
   (if (instance? OWLLiteral literal)
     literal
     (tawny.owl/literal literal))))

(.addMethod hasvalue :data data-hasvalue)


(defmontfn data-exactly [o number property]
  (.getOWLDataExactCardinality
   ontology-data-factory
   number (ensure-data-property o property)))

(.addMethod exactly :data data-exactly)

(defmontfn data-atmost [o number property]
  (.getOWLDataMaxCardinality
   ontology-data-factory number
   (ensure-data-property o property)))

(.addMethod atmost :data data-atmost)

(defmontfn data-atleast [o number property]
  (.getOWLDataMinCardinality
   ontology-data-factory number
   (ensure-data-property o property)))

(.addMethod atleast :data data-atleast)


(defn owlmin [from]
  (.getOWLDatatypeMinExclusiveRestriction
   ontology-data-factory from))

(defn owlmax [to]
  (.getOWLDatatypeMaxExclusiveRestriction
   ontology-data-factory to))

(defn minmax
  [from to]
  (.getOWLDatatypeMinMaxInclusiveRestriction
   ontology-data-factory from to))

(defn mininc [from]
  (.getOWLDatatypeMinInclusiveRestriction
   ontology-data-factory from))

(defn maxinc [to]
  (.getOWLDatatypeMaxInclusiveRestriction
   ontology-data-factory to))

(defn minmaxinc
  [from to]
  (.getOWLDatatypeMinMaxExclusiveRestriction
   ontology-data-factory from to))

(defmacro span
  [comparitor & args]
  (cond
   (= comparitor '<)
   `(apply owlmax '~args)
   (= comparitor '>)
   `(apply owlmin '~args)
   (= comparitor '><)
   `(apply minmax '~args)
   (= comparitor '<=)
   `(apply maxinc '~args)
   (= comparitor '>=)
   `(apply mininc '~args)
   (= comparitor '>=<)
   `(apply minmaxinc '~args)
   :default
   (throw (IllegalArgumentException. (str "Unknown comparitor" comparitor)))))


(defmontfn data-getfact [o property from to]
  (.getOWLDataPropertyAssertionAxiom
   ontology-data-factory
   (ensure-data-property o property) from to))

(.addMethod getfact :data data-getfact)

(defmontfn data-getfactnot [o property from to]
  (.getOWLNegativeDataPropertyAssertionAxiom
   ontology-data-factory
   (ensure-data-property o property) from to))

(.addMethod getfactnot :data data-getfactnot)
