(ns ^:slow
  tawny.render-sio-test
  (:require
   [clojure.java.io]
   [tawny owl render])
  (:use [clojure.test])
)

(defn read-sio []
  (tawny.owl/remove-ontology-maybe
   (org.semanticweb.owlapi.model.OWLOntologyID.
    (tawny.owl/iri "http://semanticscience.org/ontology/sio.owl")))
  (.loadOntologyFromOntologyDocument
   (tawny.owl/owl-ontology-manager)
   (tawny.owl/iri (clojure.java.io/resource "sio.owl"))))

(declare sio)
(declare sio-rendered)

(defn render-sio [tests]
  (def sio (read-sio))
  ;; sio-header just does namespaces and clojure stuff
  (spit "dev-resources/sio_rendered.clj" "(in-ns 'sio-header)\n")
  ;; render the ontology form and put it into a var
  (spit "dev-resources/sio_rendered.clj"
          (str
           "(def sio-rendered "
           (pr-str
            (tawny.render/as-form sio :explicit true))
           ")\n")
          :append true)
  ;; make it the default
  (spit "dev-resources/sio_rendered.clj"
          (str
           "(ontology-to-namespace (find-ns 'sio-header) sio-rendered)\n")
          :append true)
  ;; render the entire ontology
  (doseq [n
          (.getSignature sio)]
    (spit "dev-resources/sio_rendered.clj"
          (str
           (pr-str
            (tawny.render/as-form n :explicit true))
           "\n")
          :append true))
  (load-file "dev-resources/sio_header.clj")
  (def sio-rendered (eval 'sio-header/sio-rendered))
  (tests)
  (tawny.owl/remove-ontology-maybe (.getOntologyID sio-rendered))
  (tawny.owl/remove-ontology-maybe (.getOntologyID sio)))

(use-fixtures :once render-sio)

(deftest ontologies
  (is
   (instance? org.semanticweb.owlapi.model.OWLOntology sio))
  (is
   (instance? org.semanticweb.owlapi.model.OWLOntology sio-rendered)))

(deftest signature
  (is
   (=
    (do
      (require 'clojure.set)
      ;; (println
      ;;  "Difference in signature:"
      ;;  (clojure.set/difference (into (hash-set) (.getSignature sio))
      ;;                          (into (hash-set) (.getSignature sio-rendered))))
      ;; (let [s (into (sorted-set) (.getSignature sio))
      ;;       r (into (sorted-set) (.getSignature sio-rendered))
      ;;       ]
      ;;   (doseq [i
      ;;           (map vector s r)]
      ;;     (println (get i 0) "\t" (get i 1))))
      ;; (print "sio:" (count (.getSignature sio)) "\n")
      ;; (print "ren:" (count (.getSignature sio-rendered)) "\n")
      (count (remove #(.isBuiltIn %) (.getSignature sio))))
    (count (remove #(.isBuiltIn %) (.getSignature sio-rendered))))))

(deftest classes
  (is
   (= (count (.getClassesInSignature sio))
      (dec (count (.getClassesInSignature sio-rendered))))))

(deftest object
   (is
    (= (count (.getObjectPropertiesInSignature sio))
       (count (.getObjectPropertiesInSignature sio-rendered)))))
