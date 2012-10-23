(ns owl.owl
  (:require [owl.util :as util])
  (:refer-clojure :exclude [some only comment])
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget)
   (org.semanticweb.owlapi.util DefaultPrefixManager)
   (java.io ByteArrayOutputStream FileOutputStream PrintWriter)
   (java.io File)
   (org.semanticweb.owlapi.model AddAxiom)))

(def
  ^{:doc "A java object which is the main factory for all other objects"
    :private true}
  ontology-data-factory
  (OWLManager/getOWLDataFactory))


(declare current-ontology)

(defrecord
    ^{:doc "Key data about an ontology.
iri is the IRI for the ontology
file is the location that it will be saved in
manager is an OWLOntologyManager from which the ontology comes
ontology is an object of OWLOntology.
"
      :private true
      }
    Ontology [iri file prefix manager ontology])

(defn ontology [& args]
  (println  args )
  (let [options (apply hash-map args)
        iri (IRI/create (:iri options))
        manager (OWLManager/createOWLOntologyManager)]
    (Ontology.
     iri (:file options) (:prefix options) manager
     (.createOntology manager iri))))

(defmacro defontology
  "Define a new ontology with `name'. 

The following keys must be supplied. 
:iri -- the IRI for the new ontology
:file -- the file associated with the new ontology
:prefix -- the prefix used in the serialised version of the ontology
"
  [name & body]
  `(do
     (let [ontology# (ontology ~@body)]
       (def ~name ontology#)
       (owl.owl/set-current-ontology ontology#)
       ontology#
       )))

(defn set-current-ontology
  "Sets the current ontology as defined by `defontology'"
  [ontology]
  ;; check type
  ;; is this really the only way to set a variable in clojure?
  ;; do I need to do the dynamic or what?
  (def ^{:dynamic true
       :doc
       "The current ontology is either the last one defined using `defontology'
or set using `set-current-ontology'"
}
  current-ontology ontology))

(defn get-current-ontology[]
  "Gets the current ontology"
  (when (nil? current-ontology)
    (throw (IllegalStateException. "Current ontology has not been set")))
  current-ontology)

(defn- get-current-jontology[]
  "Gets the object representing the current ontology"
  (:ontology (get-current-ontology)))

(defn get-current-iri[]
  "Gets the current IRI"
  (let [iri (:iri (get-current-ontology))]
    (when (nil? iri)
      (throw (IllegalStateException. "Current ontology IRI has not been set")))
    iri))

(defn get-current-file []
  "Gets the current file"
  (let [file (:file (get-current-ontology))]
    (when (nil? file)
      (throw (IllegalStateException. "Current ontology file has not been set")))
    file))

(defn- get-current-manager[]
  "Get the OWLOntologyManager object for the current ontology"
  (let [manager (:manager (get-current-ontology))]
    (when (nil? manager)
      (throw (IllegalStateException. "No current ontology manager")))
    manager))

(defn get-current-prefix []
  "Gets the current prefix"
  (let [prefix (:prefix (get-current-ontology))]
    (when (nil? prefix)
      (throw (IllegalStateException. "No current prefix")))
    prefix))

(defn save-ontology
  "Save the current ontology in the file returned by `get-current-file'.
or `filename' if given. 
"
  ([]
     (save-ontology (:file (get-current-ontology))))
  ([filename]
     (let [file (new File filename)
           output-stream (new FileOutputStream file)
           file-writer (new PrintWriter output-stream)
           manchester-format (ManchesterOWLSyntaxOntologyFormat.)
           format (.getOntologyFormat (get-current-manager)
                                      (get-current-jontology))
           ]
       (.println file-writer "## This file was created by Clojure-OWL" )
       (.println file-writer "## It should not be edited by hand" )
       (.flush file-writer)
       (.setPrefix manchester-format (get-current-prefix)
                   (str (.toString (get-current-iri)) "#"))
       (.saveOntology (get-current-manager) (get-current-jontology)
                      manchester-format output-stream))))

(defn- iriforname [name]
  (IRI/create (str (get-current-iri) "#" name)))

(defn- get-create-object-property [name]
  (.getOWLObjectProperty ontology-data-factory
                         (iriforname name)))

(defn- ensure-object-property [prop]
  (cond (instance? org.semanticweb.owlapi.model.OWLObjectProperty prop)
        prop
        (string? prop)
        (get-create-object-property prop)))

(defn- get-create-class [name]
  (.getOWLClass ontology-data-factory
                (iriforname name)))

(defn- ensure-class [clz]
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  (cond (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
        clz
        (string? clz)
        (get-create-class clz)))

(defn- add-one-frame
  "Adds a single frame to the ontology.

OWL isn't actually frame based, even if Manchester syntax is. My original
intention is that this would be suitable for adding frame in to the ontology
but in practice this doesn't work, as not everything is an axiom. 
"
  [frame-adder name frame]
  (let [clazz (ensure-class name)]
    (.applyChange (get-current-manager)
                  (new AddAxiom (get-current-jontology)
                       (frame-adder clazz frame)))
    clazz))
  
(defn- add-frame
"Adds frames with multiple objects to the ontology"
  [frame-adder name frame]
  (first 
   (doall
    (map (fn[x]
           (add-one-frame frame-adder name x))
         frame))))

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
     (add-frame create-subclass-axiom name
                subclass)))

(defn- create-equivalent-axiom [clazz equivalent]
  (.getOWLEquivalentClassesAxiom
   ontology-data-factory
   clazz
   (ensure-class equivalent)))

(defn add-equivalent
  ([name equivalent]
     (add-frame create-equivalent-axiom name equivalent)))

(defn- create-class-axiom [clazz _]
  (.getOWLDeclarationAxiom
   ontology-data-factory
   clazz))

(defn add-class[name]
  (add-one-frame create-class-axiom name ""))

;; object properties
(defn objectproperty
  [name]
  (let [property (get-create-object-property name)
        axiom (.getOWLDeclarationAxiom
                      ontology-data-factory property)]
    (.applyChange (get-current-manager)
                  (new AddAxiom (get-current-jontology)
                       axiom))
    property))

(defmacro defoproperty [property]
  `(let [property-name# (name '~property)
         property# (owl.owl/objectproperty property-name#)]
     (def ~property property#)
     property#))



;; restrictions!
(defn some [property class]
  (.getOWLObjectSomeValuesFrom
   ontology-data-factory
   (ensure-object-property property)
   (ensure-class class)))

(defn only [property class]
  (.getOWLObjectAllValuesFrom
   ontology-data-factory
   (ensure-object-property property)
   (ensure-class class)))

;; annotations
(defn add-annotation
  [name annotation-list]
  (dorun
   (map
    (fn[annotation]
      (.applyChange (get-current-manager)
                    (new AddAxiom (get-current-jontology)
                         (.getOWLAnnotationAssertionAxiom
                          ontology-data-factory
                          (.getIRI (get-create-class name))
                          annotation))))
    annotation-list)))

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

(def comment
  (partial annotation (.getRDFSComment ontology-data-factory)))

(def isdefinedby
  (partial annotation (.getRDFSIsDefinedBy ontology-data-factory)))

(def seealso
  (partial annotation (.getRDFSSeeAlso ontology-data-factory)))

(defn owlclass-explicit
  ([name frames]
     (let [class (add-class name)]
       (add-subclass name (:subclass frames))
       (add-equivalent name (:equivalent frames))
       (add-annotation name (:annotation frames))
       class))
  ([name]
     (owlclass-explicit name {})))

(defn owlclass
  ([name & frames]
     (owlclass-explicit
      name (util/hashify frames))))

(defmacro defclass [classname & frames]
  `(let [string-name# (name '~classname)
         class# (owl.owl/owlclass string-name# ~@frames)]
     (def ~classname class#)
     class#))

