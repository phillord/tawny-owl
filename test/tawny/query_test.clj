(ns tawny.query-test
  (:require [tawny.owl :as o]
            [tawny.english :as e]
            [tawny.query :as q]
            [clojure.core.logic :as l]
            [tawny.debug]
            [tawny.test-util :refer :all])
  (:use [clojure.test]))


(def to nil)

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/" :prefix "iri" :noname true))))

(defn createandsavefixture[test]
  (let [exp
        #(throw (Exception. "default ontology used"))
        trace
        #(tawny.debug/tracing-println "default ontology used")
        ]
    (when true
      (tawny.util/add-hook
       o/default-ontology-hook exp
       ))
    (when false
      (tawny.util/add-hook
       o/default-ontology-hook trace))
    (createtestontology)
    (test)
    (tawny.util/remove-hook o/default-ontology-hook exp)
    (tawny.util/remove-hook o/default-ontology-hook trace)))

(use-fixtures :each createandsavefixture)

;;(createtestontology)

(deftest sig-1 []
  (is-set=
   (o/owl-class to "A")
   (q/signature to)))

(deftest sig-2 []
  (is-set-not=
   (do (o/owl-class to "a")
       (o/owl-class to "b"))
   (q/signature to)))

(deftest sig-3 []
  (is
   (let [o1 (o/ontology)
         _   (o/owl-import to o1)
         a1 (o/owl-class o1 "A1")
         a (o/owl-class to "A")
         sig (q/map-imports q/signature to)]
     ;; the import clojure contains lots of stuff from the tawny ontology and
     ;; other places.
     (and
      (get sig a)
      (get sig a1)))))

(deftest ann-props []
  (is-set=
   (o/annotation-property to "a")
   (q/ann-props to))
  (is-set=
   (do
     (o/owl-class to "b")
     (o/annotation-property to "a"))
   (q/ann-props to)))

(deftest anonymous
  (is
   (q/anonymous (o/anonymous-individual))))

(deftest classes []
  (is-set=
   (o/owl-class to "a")
   (q/classes to))
  (is-set=
   (do
     (o/annotation-property to "b")
     (o/owl-class to "a"))
   (q/classes to)))

(deftest data-props []
  (is-set=
   (o/datatype-property to "a")
   (q/data-props to))
  (is-set=
   (do
     (o/owl-class to "b")
     (o/datatype-property to "a"))
   (q/data-props to)))

(deftest data-types []
  (is
   (do
     (o/owl-class to "a"  :label "hello")
     (.isRDFPlainLiteral
      (first (q/data-types to))))))

(deftest direct-imports []
  (is-set=
   (let [a (o/ontology)]
     (o/owl-import to a)
     a)
   (q/direct-imports to)))

(deftest individuals
  (is-set=
   (o/individual to "i")
   (q/individuals to)))

(deftest obj-props
  (is-set=
   (o/object-property to "p")
   (q/obj-props to)))




;; Core logic additions
(defn read-sio []
  (tawny.owl/remove-ontology-maybe
   (org.semanticweb.owlapi.model.OWLOntologyID.
    (tawny.owl/iri "http://semanticscience.org/ontology/sio.owl")))
  (.loadOntologyFromOntologyDocument
   (tawny.owl/owl-ontology-manager)
   (tawny.owl/iri (clojure.java.io/resource "sio.owl"))))

(declare sio)
(defn sio-read [tests]
  (alter-var-root
   #'sio
   (fn [x] (read-sio)))
  (tests)
  (o/remove-ontology-maybe (.getOntologyID sio))
  (alter-var-root
   #'sio (constantly nil)))


(defn fetch-doctor [sio]
  (q/into-map
   (first
    (filter #(= (o/iri "http://semanticscience.org/resource/SIO_000394")
                (.getIRI %))
            (.getSignature sio)))))

;; doctor is a handy class which looks like this...
(comment
  {:super
   ((:iri "http://semanticscience.org/resource/SIO_000716")
    (:some
     (:iri "http://semanticscience.org/resource/SIO_000228")
     (:iri "http://semanticscience.org/resource/SIO_000713")))
   :type
   (:class)
   :annotation
   ((:annotation
     (:iri "http://purl.org/dc/terms/description")
     ;; this bit is elided
     (:literal "a doctor is an individual who practices medicine,...." :lang "en"))
    (:label
     (:literal "doctor" :lang "en")))})

(use-fixtures :once sio-read)

(deftest sio-exists
  (is sio)
  (is (instance? org.semanticweb.owlapi.model.OWLOntology sio)))

;; parsing all works!
(deftest sio-into-map
  (is
   (map
    q/into-map
    (.getSignature sio))))

(deftest type-match
  (is
   (= :class
      (first
       (l/run*
        [q]
        (q/matcher
         (fetch-doctor sio)
         {:type [q]})))))
  (is
   (= :class
    (first
     (l/run* [q]
      (q/typeo
       (fetch-doctor sio)
       q))))))

(deftest super-match
  (is
   (= "http://semanticscience.org/resource/SIO_000716"
      (first
       (l/run*
        [q]
        (l/fresh
         [y]
         (q/matcher
          (fetch-doctor sio)
          {:super [y]})
         (l/== [:iri q] y))))))
  (is
   (= "http://semanticscience.org/resource/SIO_000716"
      (first
       (l/run*
        [q]
        (l/fresh
         [y]
         (q/supero
          (fetch-doctor sio) y)
         (l/== [:iri q] y)))))))
