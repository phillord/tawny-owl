(ns tawny.query-test
  (:require [tawny.owl :as o]
            [tawny.english :as e]
            [tawny.query :as q]
            [clojure.core.logic :as l])
  (:use [clojure.test]))

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
