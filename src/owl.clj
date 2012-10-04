
(ns owl
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget)
   (java.io ByteArrayOutputStream)
   (java.io File)
   (org.semanticweb.owlapi.model AddAxiom)))

(defn create-ontology
  [iri]
  (let [ontology-manager (OWLManager/createOWLOntologyManager)
        ontology (.createOntology ontology-manager iri)]
    ontology))

(def ontology-data-factory (OWLManager/getOWLDataFactory))
(def ontology-iri (IRI/create "http://www.semanticweb.org/ontologies/ont.owl"))


(defn reset-ontology []
  (def ontology-manager (OWLManager/createOWLOntologyManager))
  (def ontology (.createOntology ontology-manager ontology-iri)))

(reset-ontology)

 (defn save-ontology
   []
   (let [file (new File "temp.omn")
         document-iri (IRI/create file)]
     (.saveOntology ontology-manager ontology
                    (new ManchesterOWLSyntaxOntologyFormat) document-iri)))

(defn get-create-class [name]
  (.getOWLClass ontology-data-factory
                (IRI/create (str ontology-iri "#" name))))


(defn one-or-single [_ _ frame]
  (cond (string? frame)
        :string
        (vector? frame)
        :vector
        (instance? org.semanticweb.owlapi.model.OWLClassExpression frame)
        :owlclass
        (nil? frame)
        :nil))


(defmulti add-frame (fn [frame-adder name frame]
                      (one-or-single nil nil frame)))

(defmethod add-frame :owlclass
  [frame-adder name frame]
  (.applyChange ontology-manager
                (new AddAxiom ontology
                     (frame-adder name frame))))

(defmethod add-frame :string
  [frame-adder name frame]
  (add-frame frame-adder name (get-create-class frame)))



(defmethod add-frame :vector
  [frame-adder name frame]
  (dorun
   (map (fn [x]
          (add-frame frame-adder name x))
        frame)))

(defmethod add-frame :nil
  [frame-adder name frame]
  ;; this can happen, and it's fine
  )
  



(defn create-subclass-axiom [name subclass]
  (.getOWLSubClassOfAxiom
   ontology-data-factory
   (get-create-class name)
   subclass))

(defn add-subclass
  ([name subclass]
     (add-frame create-subclass-axiom name
                subclass)))
  
(defn create-equivalent-axiom [name equivalent]
  (.getOWLEquivalentClassesAxiom
   ontology-data-factory
   (get-create-class name)
   equivalent))

(defn add-equivalent
  ([name equivalent]
     (add-frame create-equivalent-axiom name equivalent)))
      
(defn create-class-axiom [name _]
  (.getOWLDeclarationAxiom
   ontology-data-factory
   (get-create-class name)))

(defn add-class[name]
  (add-frame create-class-axiom name ""))
             

(defn owlclass
  ([name frames]
     (add-class name)
     (add-subclass name (:subclass frames))
     (add-equivalent name (:equivalent frames)))
  ([name]
     (owlclass name {})))
  

(defn objectproperty
  [name]
  (.applyChange ontology-manager
                (new AddAxiom ontology
                     (.getOWLDeclarationAxiom
                      ontology-data-factory
                      (.getOWLObjectProperty ontology-data-factory
                                             (IRI/create (str ontology-iri "#" name)))))))
