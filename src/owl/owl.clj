
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
(def ontology-iri nil)
(def ontology-manager nil)
(def ontology nil)

(def




(defn reset-ontology []
  (def ontology-manager (OWLManager/createOWLOntologyManager))
  (def ontology (.createOntology ontology-manager ontology-iri)))

(reset-ontology)

(defn save-ontology
  ([]
     (save-ontology "temp.omn"))
  ([filename]
     (let [file (new File "temp.omn")
           document-iri (IRI/create file)]
       (.saveOntology ontology-manager ontology
                      (new ManchesterOWLSyntaxOntologyFormat) document-iri))))

(defn get-create-object-property [name]
  (.getOWLObjectProperty ontology-data-factory
                         (IRI/create (str ontology-iri "#" name))))


(defn get-create-class [name]
  (.getOWLClass ontology-data-factory
                (IRI/create (str ontology-iri "#" name))))

(defn ensure-class [clz]
  (cond (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
        clz
        (string? clz)
        (get-create-class clz)))

(defn add-one-frame
  [frame-adder name frame]
  (.applyChange ontology-manager
                (new AddAxiom ontology
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
  (.applyChange ontology-manager
                (new AddAxiom ontology
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
      (.applyChange ontology-manager
                    (new AddAxiom ontology
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

