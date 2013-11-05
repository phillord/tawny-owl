(ns tawny.render-sio-test
  (:use [tawny.owl])
  (:use [clojure.test])
  (:require [tawny.render])
  )

(defn read-sio []
  (tawny.owl/remove-ontology-maybe
   (org.semanticweb.owlapi.model.OWLOntologyID.
    (tawny.owl/iri "http://semanticscience.org/ontology/sio.owl")))
  (.loadOntologyFromOntologyDocument
   (owl-ontology-manager)
   (tawny.owl/iri (clojure.java.io/resource "sio.owl"))))

(declare sio)
(declare sio-rendered)

(defn render-sio [tests]
  (def sio (read-sio))
  ;; target/classes is in the classpath which makes life easier.
  (spit "target/classes/sio_rendered.clj" "(in-ns 'sio-header)\n")
  (doseq [n (.getSignature sio)]
    (binding [tawny.render/*explicit* true]
      (spit "target/classes/sio_rendered.clj"
            (str
             (tawny.render/as-form n) "\n")
            :append true)))
  (require 'sio-header)
  (def sio-rendered (eval 'sio-header/sio-rendered))
  (tests))

(use-fixtures :once render-sio)


(deftest ontologies
  (is
   (instance? org.semanticweb.owlapi.model.OWLOntology sio))
  (is
   (instance? org.semanticweb.owlapi.model.OWLOntology sio-rendered)))

(deftest signature
  (is
   (= (count (.getSignature sio))
      (count (.getSignature sio-rendered)))))


(deftest classes
  (is
   (= (count (.getClassesInSignature sio))
      ;; tawny adds an annotation property
      (count (.getClassesInSignature sio-rendered)))))

(deftest object
   (is
    (= (count (.getObjectPropertiesInSignature sio))
       ;; tawny adds an annotation property
       (count (.getObjectPropertiesInSignature sio-rendered)))))


;; this method looks like it should work, but it craps out, and strangely
;; makes render_test.clj crash as well!

;; (deftest sio-read
;;   (is
;;    (let [o (ontology :iri "bob")]
;;      (doseq [n (.getSignature (read-sio))]
;;         (with-ontology o
;;           (prn
;;            (tawny.render/as-form n))
;;           (eval
;;            (list
;;             'do
;;             '(in-ns 'tawny.render_sio_test)
;;             '(clojure.core/use 'tawny.owl)
;;             (tawny.render/as-form n)
;;             ))))
;;      (println o))))
