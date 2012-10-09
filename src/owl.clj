(ns owl
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget)
   (java.io ByteArrayOutputStream)
   (java.io File)
   (org.semanticweb.owlapi.model AddAxiom)))


;;
;; Utillity functions for improving syntax
;;
(defn groupify
  "Takes a list with keywords and values and turns all adjacent values into a single list"
  ;; entry point
  ([list]
     (groupify () list nil nil))
  ;; not an entry point!
  ([reduced-frame long-frame-left current-keyword current-val-list]
     (if-let [first-val (first long-frame-left)]
       ;; the unreduced frame is not yet empty
       (if (keyword? first-val)
         ;; so we have a keyword as the first val
         (if current-keyword 
           ;; but we not have any vals -- exception
           (if (not current-val-list)
             (throw (IllegalArgumentException. "Cannot have two adjacent keywords"))
             ;; and we have some existing keywords and values
             (groupify
              (cons current-keyword
                    (cons current-val-list
                          reduced-frame))
              (rest long-frame-left) first-val nil))
           ;; so we have a legal new keyword, so start a new list
           (groupify
            reduced-frame (rest long-frame-left)
            first-val nil))
         ;; we do not have a keyword
         (groupify reduced-frame (rest long-frame-left)
                   current-keyword (cons first-val current-val-list)))
       
       ;; else we have nothing left in the frame, so terminate
       (cons current-keyword
             (cons current-val-list
                   reduced-frame)))))


(defn hashify
  "Takes a list with alternating keyword values and returns a hash"
  [list]
  (apply
   hash-map (groupify list)))


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

(defn add-frame
  [frame-adder name frame]
  (cond (string? frame)
        (add-frame frame-adder name (get-create-class frame))
        (instance? org.semanticweb.owlapi.model.OWLClassExpression frame)
        (.applyChange ontology-manager
                      (new AddAxiom ontology
                           (frame-adder name frame)))
        (or (vector? frame)
            (seq? frame))
        (dorun (map (fn[x]
                      (add-frame frame-adder name x))
                    frame))
        (nil? frame)
        nil))


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


(def label
  (partial annotation (.getRDFSLabel ontology-data-factory)))

(def comment
  (partial annotation (.getRDFSComment ontology-data-factory)))



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
      name (hashify frames))))



(defn objectproperty
  [name]
  (.applyChange ontology-manager
                (new AddAxiom ontology
                     (.getOWLDeclarationAxiom
                      ontology-data-factory
                      (get-create-object-property name)))))




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


