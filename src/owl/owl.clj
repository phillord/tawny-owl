
(ns owl.owl
  (:require [owl.util :as util])
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget)
   (java.io ByteArrayOutputStream)
   (java.io File)
   (org.semanticweb.owlapi.model AddAxiom)))

(def ontology-data-factory (OWLManager/getOWLDataFactory))

(def ^{:dynamic true} current-ontology nil)

(defrecord Ontology [iri file manager ontology])
(defmacro defontology [name & body]
  `(do
     (let [options# (apply hash-map '~body)
           iri# (IRI/create (:iri options#))
           manager# (OWLManager/createOWLOntologyManager)]
       (println iri#)
       (println manager#)
       (def ~name
         (Ontology.
          iri#
          (:file options#)
          manager#
          (.createOntology manager# iri#))))
     (owl.owl/set-current-ontology ~name)))


(defn set-current-ontology[ontology]
  ;;check type
  (def ^{:dynamic true} current-ontology ontology))

(defn get-current-ontology[]
  (when (nil? current-ontology)
    (throw (IllegalStateException. "Current ontology has not been set")))
  current-ontology)

(defn get-current-jontology[]
  (:ontology (get-current-ontology)))

(defn get-current-iri[]
  (let [iri (:iri (get-current-ontology))]
    (when (nil? iri)
      (throw (IllegalStateException. "Current ontology IRI has not been set")))
    iri))

(defn get-current-file []
  (let [file (:file (get-current-ontology))]
    (when (nil? file)
      (throw (IllegalStateException. "Current ontology file has not been set")))
    file))

(defn get-current-manager[]
  (let [manager (:manager (get-current-ontology))]
    (when (nil? manager)
      (throw (IllegalStateException. "No current ontology manager")))
    manager))


(defn save-ontology
  ([]
     (save-ontology (:file (get-current-ontology))))
  ([filename]
     (let [file (new File filename)
           document-iri (IRI/create file)]
       (.saveOntology (get-current-manager) (get-current-jontology)
                      (new ManchesterOWLSyntaxOntologyFormat) document-iri))))

(defn get-create-object-property [name]
  (.getOWLObjectProperty ontology-data-factory
                         (IRI/create (str (get-current-iri) "#" name))))


(defn get-create-class [name]
  (.getOWLClass ontology-data-factory
                (IRI/create (str (get-current-iri) "#" name))))

(defn ensure-class [clz]
  (cond (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
        clz
        (string? clz)
        (get-create-class clz)))

(defn add-one-frame
  [frame-adder name frame]
  (.applyChange (get-current-manager)
                (new AddAxiom (get-current-jontology)
                     (frame-adder (ensure-class name) frame))))
  

(defn add-frame
  [frame-adder name frame]
  (dorun
   (map (fn[x]
          (add-one-frame frame-adder name x))
        frame)))

(defn create-subclass-axiom [clazz subclass]
  (.getOWLSubClassOfAxiom
   ontology-data-factory
   clazz
   (ensure-class subclass)))

(defn add-subclass
  ([name subclass]
     (add-frame create-subclass-axiom name
                subclass)))

(defn create-equivalent-axiom [clazz equivalent]
  (.getOWLEquivalentClassesAxiom
   ontology-data-factory
   clazz
   (ensure-class equivalent)))

(defn add-equivalent
  ([name equivalent]
     (add-frame create-equivalent-axiom name equivalent)))

(defn create-class-axiom [clazz _]
  (.getOWLDeclarationAxiom
   ontology-data-factory
   clazz))

(defn add-class[name]
  (add-one-frame create-class-axiom name ""))


;; object properties
(defn objectproperty
  [name]
  (.applyChange (get-current-manager)
                (new AddAxiom (get-current-jontology)
                     (.getOWLDeclarationAxiom
                      ontology-data-factory
                      (get-create-object-property name)))))




;; restrictions!
(defn some [property class]
  (.getOWLObjectSomeValuesFrom
   ontology-data-factory
   (get-create-object-property property)
   (get-create-class class)))

(defn only [property class]
  (.getOWLObjectAllValuesFrom
   ontology-data-factory
   (get-create-object-property property)
   (get-create-class class)))

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
     (add-class name)
     (add-subclass name (:subclass frames))
     (add-equivalent name (:equivalent frames))
     (println (:annotation frames))
     (add-annotation name (:annotation frames)))
  ([name]
     (owlclass-explicit name {})))


(defn owlclass
  ([name & frames]
     (owlclass-explicit
      name (util/hashify frames))))

