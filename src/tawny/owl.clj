;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

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

(ns tawny.owl
  (:require 
   [clojure.walk :only postwalk]
   [tawny.util :as util])

  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation
                                 OWLNamedObject OWLOntologyID
                                 OWLAnnotationProperty
                                 )
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget OWLXMLOntologyFormat
                              RDFXMLOntologyFormat)
   (org.semanticweb.owlapi.util DefaultPrefixManager OWLEntityRemover)
   (java.io ByteArrayOutputStream FileOutputStream PrintWriter)
   (java.io File)
   (java.util Collections)
   (org.semanticweb.owlapi.model AddAxiom RemoveAxiom AddImport
                                 AddOntologyAnnotation)))



;; far as I can tell, we only ever need one of these.
(def
  ^{:doc "A java object which is the main factory for all other objects"
    :private true}
  ontology-data-factory
  (OWLManager/getOWLDataFactory))


(def
  ^{:doc "The OWLOntologyManager to use"
   }
  owl-ontology-manager
  (OWLManager/createOWLOntologyManager ontology-data-factory))

;; the current ontology provides our main mutable state. Strictly, we don't
;; need to do this, but the alternative would be passing in an ontology to
;; almost every call, or running everything inside a binding. Painful. 
(def
  ^{:dynamic true
    :doc
    "The currently bound ontology"
    }
  *current-bound-ontology* nil)

(def ^{:doc "Map between namespaces and ontologies"}
  ontology-for-namespace (ref {}))

(defn named-object? [entity]
  (instance? OWLNamedObject entity))

(defn as-named-object [entity]
  (or
   (and (instance? OWLNamedObject entity)
        entity)
   (throw (IllegalArgumentException. "Expecting a named entity"))))

(def remove-ontology-hook (util/make-hook))

(defn remove-ontology-maybe [ontologyid]
  (when (.contains owl-ontology-manager ontologyid)
    (let [ontology (.getOntology owl-ontology-manager ontologyid)]
      (.removeOntology
       owl-ontology-manager ontology)
      (util/run-hook remove-ontology-hook ontology))))


(declare add-annotation)
(declare owlcomment)
(declare versioninfo)
(defn ontology [& args]
  (let [options (apply hash-map args)
        iri (IRI/create (:iri options))]
    (remove-ontology-maybe
     (OWLOntologyID. iri))
    
    (let [jontology            
          (.createOntology owl-ontology-manager iri)
          
          ontology-format
          (.getOntologyFormat
           owl-ontology-manager jontology)]

      ;; put prefix into the prefix manager
      ;; this isn't a given -- the prefix manager being returned by default 
      ;; returns true for isPrefixOWLOntologyFormat, so we can do this, but 
      ;; strictly we are depending on the underlying types. 
      (when-let [prefix (:prefix options)]
        (.setPrefix ontology-format 
                    prefix
                    (.toString iri)))

      
      ;; add annotations to the ontology
      (add-annotation 
       jontology
       (filter (comp not nil?)
               (flatten 
                (list
                 (:annotation options)
                 (when-let [c (:comment options)]
                   (owlcomment c))
                 (when-let [v (:versioninfo options)]
                   (versioninfo v))))))
      jontology)))

(defmacro defontology
  "Define a new ontology with `name'. 

The following keys must be supplied. 
:iri -- the IRI for the new ontology
:prefix -- the prefix used in the serialised version of the ontology
"
  [name & body]
  `(do
     (let [ontology# (ontology ~@body)]
       (def 
         ~(with-meta name
            (assoc (meta name)
              :owl true))
         ontology#)
       (tawny.owl/ontology-to-namespace ontology#)
       ontology#
       )))

(defn ontology-to-namespace
  "Sets the current ontology as defined by `defontology'"
  [ontology]
  (dosync (ref-set
           ontology-for-namespace
           (merge @ontology-for-namespace
                  {*ns* ontology}))))


;; ontology options -- additional knowledge that I want to attach to each
;; ontology,  but which gets junked when the ontology does. 
(def ^{:doc "Ontology options. A map on a ref for each ontology"}
  ontology-options-atom (atom {}))

(declare get-current-ontology)
;; return options for ontology -- lazy (defn get-ontology-options [ontology])
(defn ontology-options
  "Returns the ontology options for an ontology"
  ([]
     (ontology-options (get-current-ontology)))
  ([ontology]
     (if-let [options
              (get @ontology-options-atom ontology)]
       options
       (get 
        (swap!
         ontology-options-atom assoc ontology (ref {}))
        ontology))))


(util/add-hook remove-ontology-hook
               (fn [ontology]
                 (dosync 
                  (swap! ontology-options-atom
                         dissoc ontology))))


(defn test-ontology
  "Define a test ontology.

This function probably shouldn't be here, but one of the consequences of
making the ontology implicit in all my functions is that playing on the repl
is a pain, as the test ontology has to be defined first. 

This defines a minimal test ontology.

"
  []
  (defontology a-test-ontology :iri "http://iri/" :prefix "test:"))

(defn get-current-ontology 
  "Gets the current ontology"
  ([]
     (get-current-ontology *ns*))
  ([ns]
     ;; if current ontology is inside a binding
     (or *current-bound-ontology*
         ;; so use the namespace bound one
         (get @ontology-for-namespace ns)
         ;; so break
         (throw (IllegalStateException. "Current ontology has not been set")))))


(defn get-iri
  "Gets the IRI for the given ontology, or the current ontology if none is given"
  ([]
     (get-iri (get-current-ontology)))
  ([ontology]
     (.getOntologyIRI
      (.getOntologyID ontology))))

(defn get-current-iri[]
  "DEPRECATED: Use 'get-iri' instead. "
  {:deprecated "0.8"}
  (get-iri))

(defn get-prefix 
  ([] (get-prefix (get-current-ontology)))
  ([ontology]
     ;; my assumption here is that there will only ever be one prefix for a given 
     ;; ontology. If not, it's all going to go wrong.
     (first 
      (keys
       (.getPrefixName2PrefixMap
        (.getOntologyFormat owl-ontology-manager 
                            ontology))))))

(defn get-current-prefix []
  "Gets the current prefix"
  {:deprecated "0.8"}
  (get-prefix))

(defn save-ontology
  "Save the current ontology in the file returned by `get-current-file'.
or `filename' if given. 
"
  ([filename]
     (save-ontology filename (ManchesterOWLSyntaxOntologyFormat.)
                    (str "## This file was created by Clojure-OWL\n"
                         "## It should not be edited by hand\n" )))
  ([filename format]
     (save-ontology filename format ""))
  ([filename format prepend]
     (let [file (new File filename)
           output-stream (new FileOutputStream file)
           file-writer (new PrintWriter output-stream)
           existingformat (.getOntologyFormat owl-ontology-manager
                                              (get-current-ontology))
           this-format
           (cond
            (= format :rdf) (RDFXMLOntologyFormat.)
            (= format :omn) (ManchesterOWLSyntaxOntologyFormat.)
            (= format :owl) (OWLXMLOntologyFormat.)
            :else format)]
       (when (.isPrefixOWLOntologyFormat this-format)
         (dorun
          (map #(.setPrefix this-format (get-prefix %)
                            (str (.toString (get-iri %)) "#"))
               (vals @ontology-for-namespace))))
       (.print file-writer prepend)
       (.flush file-writer)
       (.setPrefix this-format (get-current-prefix)
                   (str (.toString (get-current-iri)) "#"))
       (.saveOntology owl-ontology-manager (get-current-ontology)
                      this-format output-stream))))

(defn- iriforname [name]
  (IRI/create (str (get-current-iri) "#" name)))

(defn- get-create-object-property [name]
  (.getOWLObjectProperty ontology-data-factory
                         (iriforname name)))

(defn- ensure-object-property [prop]
  (cond
   (fn? prop)
   (ensure-object-property (prop))
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty prop)
   prop
   (string? prop)
   (get-create-object-property prop)
   true
   (throw (IllegalArgumentException.
           (str "Expecting an object property. Got: " prop)))))

(defn- get-create-class [name]
  (.getOWLClass ontology-data-factory
                (iriforname name)))

(defn ensure-class [clz]
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  (cond
   (fn? clz)
   (ensure-class (clz))
   (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
   clz
   (string? clz)
   (get-create-class clz)
   true
   (throw (IllegalArgumentException.
           (str "Expecting a class. Got: " clz)))))

(defn- add-axiom [axiom]
  (.applyChange owl-ontology-manager
                (AddAxiom. (get-current-ontology) axiom))
  axiom)

(defn remove-axiom [axiom]
  (.applyChange owl-ontology-manager
                (RemoveAxiom. (get-current-ontology) axiom))
  axiom)

(defn remove-entity
  "Remove from the ontology an entity created and added by
owlclass, defclass, objectproperty or defoproperty. Entity is the value
returned by these functions. 

This removes all the axioms that were added. So, for example, a form such as

   (defclass a
      :subclass b
      :equivalent c)

adds three axioms -- it declares a, makes it a subclass of b, and equivalent
of c."
  [entity]
  (let [remover
        (OWLEntityRemover. owl-ontology-manager
                           (hash-set
                            (get-current-ontology)))]
    (.accept entity remover)
    (.applyChanges owl-ontology-manager
                   (.getChanges remover))))

(defn- add-one-frame
  "Adds a single frame to the ontology.

OWL isn't actually frame based, even if Manchester syntax is. My original
intention is that this would be suitable for adding frame in to the ontology
but in practice this doesn't work, as not everything is an axiom. 
"
  [frame-adder name frame]
  (let [clazz (ensure-class name)
        axiom (frame-adder clazz frame)]
    (add-axiom axiom)
    axiom))
  
(defn- add-frame
"Adds frames with multiple objects to the ontology"
  [frame-adder name frame]
  (doall
   (map (fn[x]
          (add-one-frame frame-adder name x))
        ;; owlsome, only, someonly return lists
        (flatten frame))))

(defn- create-subclass-axiom
  "Creates a subclass axiom for the given class and subclass.

The class needs to be a OWLClass object, while the subclass can be a string,
class, or class expression. "
  [clazz subclass]
  (.getOWLSubClassOfAxiom
   ontology-data-factory
   clazz
   (ensure-class subclass)))

(defn add-subclass
"Adds a specific class to the ontology"
  ([name subclass]
     (add-frame create-subclass-axiom
                name
                subclass)))

(defn- create-equivalent-axiom [clazz equivalent]
  (.getOWLEquivalentClassesAxiom
   ontology-data-factory
   clazz
   (ensure-class equivalent)))

(defn add-equivalent
  [name equivalent]
  {:pre [(or (nil? equivalent) 
             (seq? equivalent))]}
  (add-frame create-equivalent-axiom name equivalent))

(defn- create-class-axiom [clazz _]
  (.getOWLDeclarationAxiom
   ontology-data-factory
   clazz))

(defn add-disjoint-union [clazz subclasses]
  (let [ensured-subclasses
        (doall (map #(ensure-class %) subclasses))
        ]
    (list
     (add-axiom
      (.getOWLDisjointUnionAxiom
       ontology-data-factory
       (ensure-class clazz)
       (java.util.HashSet.  ensured-subclasses))))))

(defn add-class[name]
  (add-one-frame create-class-axiom name ""))

(defn add-domain [property domainlist]
  (let [property (ensure-object-property property)]
    (doall
     (map
      (fn [domain]
        (add-axiom
         (.getOWLObjectPropertyDomainAxiom
          ontology-data-factory property
          (ensure-class domain))))
      domainlist))))

(defn add-range [property rangelist]
  (let [property (ensure-object-property property)]
    (doall
     (map
      (fn [range]
        (add-axiom
         (.getOWLObjectPropertyRangeAxiom
          ontology-data-factory property
          (ensure-class range))))
      rangelist))))

(defn add-inverse [property inverselist]
  (let [property (ensure-object-property property)]
    (doall
     (map
      (fn [inverse]
        (add-axiom
         (.getOWLInverseObjectPropertiesAxiom
          ontology-data-factory property
          (ensure-object-property inverse))))
      inverselist))))


(defn add-superproperty [property superpropertylist]
  (let [property (ensure-object-property property)]
    (doall
     (map
      (fn [superproperty]
        (add-axiom
         (.getOWLSubObjectPropertyOfAxiom
          ontology-data-factory property
          (ensure-object-property superproperty))))
      superpropertylist))))


;; Really it would make more sense to use keywords, but this breaks the
;; groupify function which expects alternative keyword value args. The
;; approach of using strings and symbol names here is scary -- if someone does
;; (defclass transitive) for example, it's all going to break. I don't think
;; that the const does what it might

;; this is my second attempt -- a enum for a dynamic world. 

(def ^:const transitive "transitive")
(def ^:const functional "functional")
(def ^:const inversefunctional "inversefunctional")

(def
  ^{:private true}
  charfuncs
  {transitive #(.getOWLTransitiveObjectPropertyAxiom %1 %2)
   functional #(.getOWLFunctionalObjectPropertyAxiom %1 %2)
   inversefunctional #(.getOWLInverseFunctionalObjectPropertyAxiom %1 %2)
   })

(defn add-characteristics [property characteristics]
  (doall
   (map
    (fn [x]
      (when-not (get charfuncs x)
        (throw (IllegalArgumentException. "Characteristic is not recognised:" x)))
      (add-axiom
       ((get charfuncs x)
        ontology-data-factory (ensure-object-property property))))
    characteristics)))

(def
  ^{:dynamic true}
  *default-frames* nil)

(def
  ^{:doc "Axioms we have added recently"
    :dynamic true}
  recent-axiom-list
  nil)

;; object properties
(defn objectproperty-explicit
  [name {:keys [domain range inverseof subpropertyof characteristics] :as all}]
  (let [property (get-create-object-property name)
        axioms
        (concat 
         (list (add-axiom
                (.getOWLDeclarationAxiom
                 ontology-data-factory property)))
         (add-domain property domain)
         (add-range property range)
         (add-inverse property inverseof)
         (add-superproperty property subpropertyof)
         (add-characteristics property characteristics)
         )]
    ;; store classes if we are in an inverse binding
    (when (seq? recent-axiom-list)
      (set! recent-axiom-list
            (concat (list property) recent-axiom-list)))
    property))


(defn objectproperty
  [name & frames]
  (objectproperty-explicit
   name
   (util/check-keys
    (merge-with concat
                (util/hashify frames)
                *default-frames*)
    [:domain :range :inverseof :subpropertyof :characteristics])))

(defmacro defoproperty [property & frames]
  `(let [property-name# (name '~property)
         property# (tawny.owl/objectproperty property-name# ~@frames)]
     (def ~property property#)
     property#))

;; restrictions! name clash -- we can do nothing about this, so accept the
;; inconsistency and bung owl on the front.


(defn owlsome
  ;; handle the single case
  ([property class]
     (.getOWLObjectSomeValuesFrom
      ontology-data-factory
      (ensure-object-property property)
      (ensure-class class)))
  ;; handle the many case
  ([property class & classes]
     (doall
      (map (partial owlsome property)
           (cons class classes)))))

(defn only
  ([property class]
     (.getOWLObjectAllValuesFrom
      ontology-data-factory
      (ensure-object-property property)
      (ensure-class class)))
  ([property class & classes]
     (doall (map (partial only property)
                 (cons class classes)))))

;; long shortcut -- for consistency with some
(def owlonly only)


;; forward declaration
(declare owlor)
(defn someonly [property & classes]
  (list
   (apply 
    owlsome 
    (concat 
     (list property) classes))
   
   (only property 
         (apply owlor classes))))


;; union, intersection
(defn owland [& classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owland must have at least two classes")))

    (.getOWLObjectIntersectionOf
     ontology-data-factory
     (java.util.HashSet. 
      (doall (map
              #(ensure-class %)
              ;; flatten list for things like owlsome which return lists
              classes))))))

;; short cuts for the terminally lazy. Still prefix!
(def && owland)

(defn owlor [& classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owlor must have at least two classes")))

    (.getOWLObjectUnionOf
     ontology-data-factory
     (java.util.HashSet.
      (doall (map #(ensure-class %)
                  (flatten classes)))))))

(def || owlor)

(defn owlnot [class]
  (.getOWLObjectComplementOf
   ontology-data-factory
   (ensure-class class)))

(def ! owlnot)

;; cardinality
(defn atleast [cardinality property class]
  (.getOWLObjectMinCardinality
   ontology-data-factory cardinality
   (ensure-object-property property)
   (ensure-class class)))

(defn atmost [cardinality property class]
  (.getOWLObjectMaxCardinality
   ontology-data-factory cardinality
   (ensure-object-property property)
   (ensure-class class)))

(defn exactly [cardinality property class]
  (.getOWLObjectExactCardinality
   ontology-data-factory cardinality
   (ensure-object-property property)
   (ensure-class class)))

(declare ensure-individual)
(defn
  oneof [& individuals]
  (.getOWLObjectOneOf
   ontology-data-factory
   (java.util.HashSet.
    (doall
     (map #(ensure-individual %)
          (flatten individuals))))))
  
;; annotations
(defmulti add-an-annotation (fn [named-entity _] 
                              (class named-entity)))

(defmethod add-an-annotation OWLNamedObject
  [named-entity annotation]
  ;; get the axiom and apply it. 
  (let [axiom
        (.getOWLAnnotationAssertionAxiom
         ontology-data-factory
         (.getIRI named-entity) annotation)]
    (add-axiom axiom)))

(defmethod add-an-annotation OWLOntology
  [ontology annotation]
  ;; we don't appear to need an axiom to add annotation to the ontology. 
  (.applyChange 
   owl-ontology-manager 
   (AddOntologyAnnotation. ontology annotation)))

(defn add-annotation
  [named-entity annotation-list]
  (doall
   (map
    (partial add-an-annotation named-entity)
    annotation-list)))

(defn ensure-annotation [property]
  (cond 
   (instance? OWLAnnotationProperty property)
   property
   (instance? IRI property)
   (.getOWLAnnotationProperty 
    ontology-data-factory property)
   (instance? String property)
   (ensure-annotation
    (IRI/create property))
   :default 
   (throw (IllegalArgumentException.
           (format "Expecting an OWL annotation property: %s" property)))))

(defn annotation
  ([annotation-property literal]
     (annotation annotation-property literal "en"))
  ([annotation-property literal language]
     (.getOWLAnnotation 
      ontology-data-factory
      annotation-property 
      (.getOWLLiteral ontology-data-factory literal language))))

;; various annotation types
(def label
  (partial annotation (.getRDFSLabel ontology-data-factory)))

(def owlcomment
  (partial annotation (.getRDFSComment ontology-data-factory)))

(def isdefinedby
  (partial annotation (.getRDFSIsDefinedBy ontology-data-factory)))

(def seealso
  (partial annotation (.getRDFSSeeAlso ontology-data-factory)))

(def backwardcompatiblewith
  (partial annotation (.getOWLBackwardCompatibleWith ontology-data-factory)))

(def incompatiblewith
  (partial annotation (.getOWLIncompatibleWith ontology-data-factory)))

(def versioninfo
  (partial annotation (.getOWLVersionInfo ontology-data-factory)))

(defn annotation-property [property]
  (.getOWLAnnotationProperty 
   ontology-data-factory
   (iriforname property)))

(defmacro defannotationproperty
  [property]
  `(let [property-name# (name '~property)
         (property# (tawny.owl/annotation-property property-name#))]
     (def ~property property#)))


;; data type properties
(defn datatypeproperty [name & frames]
  (throw (Exception. "Not written this yet")))




(defn owlclass-explicit
  ([name frames]
     (let [classname (or (first (:name frames)) name)
           class
           (ensure-class classname)]
       ;; store classes if we are in a disjoint binding
       (when (seq? recent-axiom-list)
         (set! recent-axiom-list
               (concat (list class)
                       recent-axiom-list)))
       ;; create the class
       (do
         ;; add-class returns a single axiom -- concat balks at this
         (add-class class)
         (add-subclass class (:subclass frames))
         (add-equivalent class (:equivalent frames))
         (add-annotation class (:annotation frames))
         ;; change these to add to the annotation frame instead perhaps?
         (when (:comment frames)
           (add-annotation class
                           (list (owlcomment
                                  (first (:comment frames))))))

         (when (:label frames)
           (add-annotation class
                           (list (label
                                  (first
                                   (:label frames))))))
         ;; return the class object
         class)))
  ([name]
     (owlclass-explicit name {})))


(defn owlclass
  ([name & frames]
     (owlclass-explicit
      name
      (util/check-keys
       (merge-with
               concat
               (util/hashify frames)
               *default-frames*)
       [:subclass :equivalent :annotation :name :comment :label]))))

(defmacro defclass [classname & frames]
  `(let [string-name# (name '~classname)
         class# (tawny.owl/owlclass string-name# ~@frames)]
     (def 
      ~(with-meta classname
         (assoc (meta classname)
           :owl true))
       class#)))


(defn disjointclasseslist [list]
  (let [classlist
        (doall
         (map
          (fn [x]
            (ensure-class x))
          list))]
    (add-axiom 
     (.getOWLDisjointClassesAxiom
      ontology-data-factory
      (into-array OWLClassExpression
                  classlist)))))
  
(defn disjointclasses [& list]
  (disjointclasseslist list))

(defn- get-create-individual [individual]
  (.getOWLNamedIndividual ontology-data-factory
                          (iriforname individual)))

(defn- ensure-individual [individual]
  (cond (instance? org.semanticweb.owlapi.model.OWLIndividual)
        individual
        (string? individual)
        (get-create-individual individual)
        true
        (throw (IllegalArgumentException.
                (str "Expecting an Inidividual. Got: " individual)))))

;; need to support all the different frames here...
;; need to use hashify
(defn individual-add-types [name types]
  (let [individual (ensure-individual name)]
    (doall
     (map
      (fn [type]
        (add-axiom
         (.getOWLClassAssertionAxiom
          ontology-data-factory
          (ensure-class type)
          individual)))
      types))
    individual))

(defn individual [name & frames]
  (let [hframes
        (util/check-keys 
         (util/hashify frames)
         [:types])]
    (individual-add-types name (:types hframes))))



;; owl imports

(defn owlimport [ontology]
  (.applyChange owl-ontology-manager
                (AddImport. (get-current-ontology)
                            (.getOWLImportsDeclaration
                             ontology-data-factory
                             (get-iri ontology)))))


;; return type of individual is buggered
(defmacro defindividual [individualname & frames]
  `(let [string-name# (name '~individualname)
         individual# (tawny.owl/individual string-name# ~@frames)]
     (def ~individualname individual#)
     individual#))

;; convienience macros
;; is this necessary? is as-disjoint-subclasses not enough?
(defmacro as-disjoint [& body]
  `(do ;; delete all recent classes
     (binding [tawny.owl/recent-axiom-list '()]
       ;; do the body
       ~@body
       ;; set them disjoint if there is more than one. if there is only one
       ;; then it would be illegal OWL2. this macro then just shields the body
       ;; from any other as-disjoint statements.
       (when (< 1 (count tawny.owl/recent-axiom-list))
         (tawny.owl/disjointclasseslist
          tawny.owl/recent-axiom-list)))))

(defmacro as-inverse [& body]
  `(do
     (binding [tawny.owl/recent-axiom-list '()]
       ~@body
       (when-not (= (count tawny.owl/recent-axiom-list) 2)
         (throw (IllegalArgumentException. "Can only have two properties in as-inverse")))
       (tawny.owl/add-inverse
        (first tawny.owl/recent-axiom-list)
        (rest tawny.owl/recent-axiom-list))
       )))


;; bind to 
(defmacro with-ontology [ontology & body]
  `(binding [tawny.owl/*current-bound-ontology* ~ontology]
     ~@body))


;; specify default frames which should be merged with additional frames passed
;; in. place into a dynamic variable and then use (merge-with concat) to do
;; the business
(defmacro with-default-frames [frames & body]
  `(binding [tawny.owl/*default-frames*
             (tawny.util/hashify ~frames)]
     ~@body))


(defmacro as-disjoint-subclasses [superclass & body]
  `(as-subclasses ~superclass :disjoint ~@body))


(defmacro as-subclasses [superclass & body]
  (let [options# (vec (take-while keyword? body))
        rest# (drop-while keyword? body)]
    `(binding [tawny.owl/recent-axiom-list '()]
       (with-default-frames [:subclass ~superclass]
        ~@body)
       (tawny.owl/subclass-options 
        ~options# 
        ~superclass
        tawny.owl/recent-axiom-list))))

(defn subclass-options 
  [options superclass subclasses]
  (let [optset (into #{} options)]
    (when (and 
           (contains? optset :disjoint)
           ;; set them disjoint if there is more than one. if there is only one
           ;; then it would be illegal OWL2. this macro then just shields the body
           ;; from any other as-disjoint statements.
           (< 1 (count tawny.owl/recent-axiom-list)))
      (disjointclasseslist
       recent-axiom-list))
    (when (and 
           (contains? optset :cover))
      (add-equivalent
       superclass 
       (list (owlor recent-axiom-list))))))

(defmacro declare-classes
  "Declares all the classes given in names.

This is mostly useful for forward declarations, but the classes declared will
have any default frames or disjoints if `as-disjoints' or
`with-default-frames' or equivalent macros are in use.

See `defclassn' to define many classes with frames.
"
  [& names]
  `(do ~@(map
          (fn [x#]
            `(defclass ~x#))
          names)))

(defmacro defclassn
  "Defines many classes at once.

Each class and associated frames should be supplied as a vector.

See `declare-classes' where frames (or just default frames) are not needed.
"
  [& classes]
  `(do ~@(map
          (fn [x#]
            `(defclass ~@x#)) classes)))

;; predicates
(defn- recurseclass?
  "Determine class relationship

Returns true if targetclass is directly or indirectly related to a class in
namelist where recursefunction returns all direct relationships"
  [namelist targetclass recursefunction]
  (and (first namelist)
       (or (= (first namelist)
              targetclass)
           (recurseclass? (recursefunction
                           (first namelist))
                          targetclass
                          recursefunction)
           (recurseclass? (rest namelist)
                          targetclass
                          recursefunction
                          ))))

(defn isuperclasses
  "Returns the direct superclasses of name.
Name can be either a class or a string name. Returns a list of classes"
  [name]
  (.getSuperClasses (ensure-class name)
                    (get-current-ontology)))


(defn superclass?
  "Returns true is name has superclass as a superclass"
  [name superclass]
  (recurseclass? (list (ensure-class name))
                 (ensure-class superclass)
                 isuperclasses))

(defn isubclasses
  "Returns the direct subclasses of name."
  [name]
  (.getSubClasses (ensure-class name)
                  (get-current-ontology)))

(defn subclass?
  "Returns true if name has subclass as a subclass"
  [name subclass]
  (recurseclass? (list (ensure-class name))
               (ensure-class subclass)
               isubclasses))

(defn- subclasses-1 [classlist]
  ;; if there are no subclasses return empty list
  (if (= 0 (count classlist))
    (list)
    (concat (list (first classlist))
            ;; can't use recur, not in tail position
            (subclasses-1 (rest classlist))
            (subclasses-1 (isubclasses (first classlist))))))

(defn subclasses [class]
  (subclasses-1 (isubclasses class)))

(defn disjoint? 
  ([a b]
     (disjoint? a b (get-current-ontology)))
  ([a b ontology]
     (contains?
      (.getDisjointClasses a ontology)
      b)))


(defn equivalent?
  ([a b]
     (equivalent? a b (get-current-ontology)))
  ([a b ontology]
     (contains? 
      (.getEquivalentClasses a ontology) 
      b)))

;; some test useful macros

;; modified from with-open
(defmacro with-probe-entities
  "Evaluate the body with a number of entities defined. Then 
delete these entities from the ontology"
  [bindings & body]
  (when-not (vector? bindings)
    (IllegalArgumentException. "with-probe-entities requires a vector"))
  (when-not (even? (count bindings))
    (IllegalArgumentException. 
     "with-probe-entities requires an even number of forms in binding vector"))
  (cond 
   (= (count bindings) 0)
   `(do ~@body)
   (symbol? (bindings 0))
   `(let ~(subvec bindings 0 2)
      (with-probe-entities
        ~(subvec bindings 2) 
        ;; try block just so we can use finally
        (try 
          ~@body
          (finally          
            (tawny.owl/remove-entity ~(bindings 0))))))
   :else
   (throw (IllegalArgumentException. 
           "with-probe-entities only allows Symbols in bindings"))))


(defmacro with-probe-axioms
  "Evaluate the body with a number of axioms. Then
delete these axioms from the ontology"
  [bindings & body]
  (when-not (vector? bindings)
    (IllegalArgumentException. "with-probe-axioms requires a vector"))
  (when-not (even? (count bindings))
    (IllegalArgumentException. 
     "with-probe-axioms requires an even number of forms in binding vector"))
  (cond 
   (= (count bindings) 0)
   `(do ~@body)
   (symbol? (bindings 0))
   `(let ~(subvec bindings 0 2)
      (with-probe-axioms
        ~(subvec bindings 2) 
        ;; try block just so we can use finally
        (try 
          ~@body
          (finally          
            (tawny.owl/remove-axiom ~(bindings 0))))))
   :else
   (throw (IllegalArgumentException. 
           "with-probe-axioms only allows Symbols in bindings"))))



(defn owlthing []
  (.getOWLThing ontology-data-factory))

(defn owlnothing []
  (.getOWLNothing ontology-data-factory))



;; add a prefix or suffix to contained defclass
(defn- alter-symbol-after-def-form [f x]
  (if (and (seq? x)
           (= (first x) 'defclass))
    `(defclass ~(f (second x))
       ~@(drop 2 x))
    x))

(defn prefix-symbol [prefix sym]
  (symbol
   (str prefix (name sym))))

(defn- suffix-symbol [suffix sym]
  (symbol 
   (str (name sym) suffix)))

(defn- alter-all-symbol-after-def-form [f x]
  (clojure.walk/postwalk
   (partial alter-symbol-after-def-form f)
   x
   ))

(defmacro with-prefix [prefix & body]
  (let [newbody 
        (alter-all-symbol-after-def-form
         (partial prefix-symbol prefix)
         body)]
    `(do ~@newbody)))


(defmacro with-suffix [suffix & body]
  (let [newbody 
        (alter-all-symbol-after-def-form
         (partial suffix-symbol suffix)
         body)]
    `(do ~@newbody)))

