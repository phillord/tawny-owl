(ns owl.test.owl
  (:refer-clojure :exclude [some only comment])
  (:require [owl.owl :as o])
  [:use clojure.test])

(defn createandsavefixture[test]
  (o/defontology testontology
    :file "test.omn"
    :iri "http://iri/"
    :prefix "iri:")  
  (test)
  (o/save-ontology)
  )

(use-fixtures :each createandsavefixture)

(deftest ontology
  (is true))

(deftest defontology
  (is (not (nil? testontology)))
  (is (= 0 (.getAxiomCount (#'o/get-current-jontology)))))

(deftest get-current-ontology
  (is (not (nil? (o/get-current-ontology))))
  (is (= testontology (o/get-current-ontology))))

(deftest set-current-ontology
  (is (do
        (let [current-ont (o/get-current-ontology)
              ;; redefines current ontology to new ont
              newont
              (o/defontology testontology2
                :file "test.omn" :iri "http://iri/" :prefix "iri2:")]
          (o/set-current-ontology current-ont)
          ;; the actual test
          (= current-ont (o/get-current-ontology))))))
        
(deftest get-current-jontology
  (is (not (nil? (#'o/get-current-jontology)))))

(deftest get-current-iri
  (is (= "http://iri/" (.toString (#'o/get-current-iri)))))

(deftest get-current-file []
  (is (= "test.omn" (o/get-current-file))))

(deftest get-current-manager []
  (is (not (nil? (#'o/get-current-manager)))))

(deftest get-current-prefix []
  (is (= "iri:" (o/get-current-prefix))))

(deftest save-ontology []
  (is (do (o/save-ontology)
          true)))

(deftest iriforname []
  (is (= (.toString (#'o/iriforname "test"))
         "http://iri/#test")))

(deftest get-create-object-property []
  (is (instance? org.semanticweb.owlapi.model.OWLObjectProperty
                 (#'o/get-create-object-property "hello"))))

(deftest get-create-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (#'o/get-create-class "hello"))))

(deftest ensure-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class "hello")))
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class (#'o/get-create-class "hello")))))

(deftest create-subclass-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLSubClassOfAxiom
                 (#'o/create-subclass-axiom
                  (#'o/ensure-class "a") "b"))))

(deftest add-one-frame []
  (is (not
       (nil?
        (#'o/add-one-frame
         #'o/create-subclass-axiom
         (#'o/ensure-class "a") "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLClass
       (#'o/add-one-frame
        #'o/create-subclass-axiom
        (#'o/ensure-class "a") "b"))))

(deftest add-frame []
  (let [frame-call
        (#'o/add-frame
         #'o/create-subclass-axiom
         (#'o/ensure-class "a") '( "b" "c" "d"))]
    (is (not (nil? frame-call)))
    (is (instance? org.semanticweb.owlapi.model.OWLClass
                   frame-call))))



(deftest create-equiavlent-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom
                 (#'o/create-equivalent-axiom
                  (#'o/ensure-class "a") "b"))))

(deftest add-equivalent []
  (let [equiv (#'o/add-equivalent
               (#'o/ensure-class "a")
               (list (#'o/ensure-class "b")))]
    (is (not (nil? equiv)))
    (is (instance? org.semanticweb.owlapi.model.OWLClass
                   equiv))))

(deftest create-class-axiom []
  (is (instance? org.semanticweb.owlapi.model.OWLDeclarationAxiom
                 (#'o/create-class-axiom (#'o/ensure-class "a") "b"))))

(deftest add-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (o/add-class "a"))))

(deftest objectproperty []
  (is (instance? org.semanticweb.owlapi.model.OWLObjectProperty
                 (o/objectproperty "b"))))



(deftest some []
  (is (not (nil? (o/some (o/objectproperty "b") "a")))))

(deftest only []
  (is (not (nil? (o/only (o/objectproperty "b") "a")))))


(deftest owlclass
  (is (= 1
         (do (o/owlclass "test")
             (.size (.getClassesInSignature
                     (#'o/get-current-jontology))))))
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (o/owlclass "test"))))


;; (subclass?
;;  (owlclass "test1")
;;  (owlclass "test"
;;            :subclass "test1")))