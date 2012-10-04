
(use 'owl)

(defmacro sav [body]
  `(do
     ~body
     (owl/save-ontology)))

(sav (objectproperty "hasPart"))

(sav (owl/owlclass "test1"
                {:subclass ["test2" "test3"]
                 :equivalent "test6"}))

(sav (owl/owlclass "test10"
                   {:subclass ["hasPart" "test15"]}))

(sav (owl/owlclass "test8"
                   {:subclass ["test9" "test10"]}))

(sav (owl/owlclass "test4"
                   {:subclass "test3"}))

(sav (owl/owlclass "test6"))

(sav (owl/owlclass "test5" {}))

;; hmmm
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass x {:subclass "test3"}))
   '("a" "b" "c" "d"))))
 
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass
      (str "HumanChromosome" x)
      {:subclass "HumanChromosome"}))
   (range 1 23))))


(owl/save-ontology)
(sav (owl/reset-ontology))

     
  
;; OWLOntologyManager m = create();
;; OWLOntology o = m.createOntology(pizza_iri);
;; // class A and class B
;; OWLClass clsA = df.getOWLClass(IRI.create(pizza_iri + "#A"));
;; OWLClass clsB = df.getOWLClass(IRI.create(pizza_iri + "#B"));
;; // Now create the axiom
;; OWLAxiom axiom = df.getOWLSubClassOfAxiom(clsA, clsB);
;; // add the axiom to the ontology.
;; AddAxiom addAxiom = new AddAxiom(o, axiom);
;; // We now use the manager to apply the change
;; m.applyChange(addAxiom);
;; // remove the axiom from the ontology
;; RemoveAxiom removeAxiom = new RemoveAxiom(o,axiom);
;; m.applyChange(removeAxiom);








;; StreamDocumentTarget target = new StreamDocumentTarget(
;;                 new ByteArrayOutputStream());
;;         m.saveOntology(o, target);
        

;;(def ontology (create-ontology "http://www.semanticweb.org/ontologies/ont.owl"))


;; (class {:name "kont:Hu1p36_3"
;;         :subclassof "kont:CytogeneticBand"})

;; (class "kont:Hu1p36_2"
;;        :subclassof "kont:CytogeneticBand")

;; (class "kont:NormalHu1"
;;        :subclassof
;;        ["kont:NormalHumanChromosome"
;;         "kont:Hu1"
;;         (exactly "kont:hasPart" 1 "kont:Hu1p36_3")])

;; (disjointclasses
;;  ["kont:Hu1p36_3", "kont:Hu1p36_2"])


;; (defn class
;;   "OWL class"
;;   [definition]
;;   (str "Class:" (:name definition)
;;        " "
;;        "SubClassOf: "
;;        (:subclassof definition)))
