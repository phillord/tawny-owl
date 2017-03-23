;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns
    ^{:doc "Read external OWL files and use them in tawny"
      :author "Phillip Lord"}
  tawny.read
  (:require [tawny owl util protocol]
            [clojure.string :only replace]
            [tawny.type :as t])
  (:refer-clojure :exclude [read])
  (:import
   (java.io File)
   (java.net URL)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.semanticweb.owlapi.model
    OWLAnnotation OWLLiteral
    IRI OWLNamedObject OWLOntologyID
    OWLOntology OWLEntity)
   (org.semanticweb.owlapi.search EntitySearcher)))

(def ^:dynamic *noisy-intern* nil)

(defn iri-starts-with-filter
  "Checks e to see if it is an OWLNamedObject and has an IRI starting with
starts-with. Use this partially applied with a filter for 'read'."
  [starts-with _ e]
  (and (t/named? e)
       (.startsWith
        (str
         (.getIRI ^OWLNamedObject e))
        starts-with)))

(defn default-filter
  "Filter for only named objects with an IRI the same as the ontology IRI."
  [o e]
  (iri-starts-with-filter
   (str (tawny.protocol/as-iri o)) nil e))

(defn default-transform
  "Extract the fragment from each IRI."
  [_ ^OWLNamedObject e]
  (.. e (getIRI) (getFragment)))

(defn filter-for-labels
  "Filter annotations on an entity for labels"
  [^OWLOntology o ^OWLEntity e]
  (filter
   #(some-> ^OWLAnnotation %
        (.getProperty)
        (.isLabel))
   (EntitySearcher/getAnnotations e o)))

(defn label-transform
  "Get text from label annotation"
  [^OWLOntology o ^OWLEntity e]
  (some-> (filter-for-labels o e)
      ^OWLAnnotation (first)
      ^OWLLiteral (.getValue)
      (.getLiteral)))

(defn noisy-nil-label-transform
 "Check for empty labels noisily"
 [o e]
 (let [trans (label-transform o e)]
    (when (nil? trans)
      (println "Unable to generate transform for:" e))
    trans))

(defn really-noisy-nil-label-transform
 "Check for empty labels noisily"
 [o e]
 (println e)
 (let [trans (label-transform o e)]
    (when (nil? trans)
      (println "Unable to generate transform for:" e))
    (println trans)
    trans))

(defn exception-nil-label-transform
 "Check for empty labels noisily"
 [o e]
  (let [trans (label-transform o e)]
    (when (nil? trans)
      (throw (IllegalArgumentException.
              (str "Unable to generate transform for:" e))))
    trans
    ))

(defn fragment-transform
  "Create an entity name from the IRI fragment"
  [^OWLNamedObject e]
  (-> e
      (.getIRI)
      (.getFragment)))

(defn stop-characters-transform
  "Takes a string and treats characters not legal in a
Clojure symbol. Use this composed with a entity transform function"
  [s]
  (let [r (clojure.string/replace s #"[() /,]" "_")
        ^Character f (first r)]
    (str
     (if (or (Character/isLetter f)
             (= \_ f))
       "" "_")
     r)))

(defn intern-entity
  "Intern the OWL entity, applying transform to the entity to generate a name
to intern."
  ([ns e]
   (intern-entity ns e default-transform))
  ([ns e transform]
     (try
       (when (t/named? e)
         (let [name
               (stop-characters-transform (transform e))]
           (when *noisy-intern*
             (println "Interning as:" name " entity: " e))
           (tawny.owl/intern-owl-string ns name e)))
       (catch IllegalArgumentException i
         (print "Broken Intern on:" e)
         (throw i)))))

(defn iri-mapper
  "Given a map of Ontology IRI strings to document IRI strings, return an
OWLOntologyIRIMapper instance."
  [iri-map]
  (proxy [org.semanticweb.owlapi.model.OWLOntologyIRIMapper] []
    (getDocumentIRI [^IRI o-iri]
      (when-let [retn (get iri-map (str o-iri))]
        (tawny.owl/iri retn)))))

(defn resource-iri-mapper
  "Given a map of Ontology IRI strings to resource strings, return an
  OWLOntologyIRIMapper instance."
  [iri-map]
  (iri-mapper
   (into {}
         (for [[k v] iri-map]
           [k (clojure.java.io/resource v)]))))

(defn read
  "Reads an ontology, and interns entities as vars. This takes a number of
keyword arguments. Arguments are,
:iri, :version-iri -- currently these must be specified in the read form,
although they will also be present in th e OWL source.
:location -- the location of the source -- this can be anything that can be
passed to .loadOntologyFromOntologyDocument on the OWLOntologyManager.
:prefix -- a prefix for the ontology
:filter -- a filter function -- only entities returning true are interned.
:transform -- entities are interned using a name returned by this function
:mapper -- an OWLOntologyIRIMapper which is to be used for loading. See
iri-mapper and resource-iri-mapper.
:namespace -- the namespace in which to intern (or *ns*)."
  [& rest]
  (let [{:keys [location iri prefix filter transform version-iri
                mapper namespace]} rest
        jiri (tawny.owl/iri iri)
        viri (if version-iri
               (tawny.owl/iri version-iri))
        ontologyid
        (OWLOntologyID. jiri viri)
        ^OWLOntology owlontology
        (do
          (tawny.owl/remove-ontology-maybe ontologyid)
          (when mapper
            (.addIRIMapper
             (tawny.owl/owl-ontology-manager)
             mapper))
          (try
            ;; use the with types thing!
            (tawny.util/with-types
              [location [java.io.File java.io.InputStream
                         IRI org.semanticweb.owlapi.io.OWLOntologyDocumentSource]]
              (.loadOntologyFromOntologyDocument
               (tawny.owl/owl-ontology-manager)
               location))
            (finally
              (when mapper
                (.removeIRIMapper
                 (tawny.owl/owl-ontology-manager)
                 mapper)))))]
    (when prefix
      (let [format (.getOntologyFormat (tawny.owl/owl-ontology-manager)
                                       owlontology)]
        (if (.isPrefixOWLOntologyFormat format)
          (.setPrefix (.asPrefixOWLOntologyFormat format)
                      prefix (str iri))
          (throw (IllegalArgumentException.
                  "Attempt to provide a prefix to an ontology that is not using a prefix format")))))
    ;; this is the ontology for the namespace so stuff it place
    (tawny.owl/ontology-to-namespace owlontology)
    ;;
    (doall
     (map
      (fn [x]
        ;; grab each entity, put classes, object properties and so forth into
        ;; current system.
        (intern-entity
         (or namespace *ns*) x
         (partial (or transform default-transform)
                  owlontology)))
      ;; filter this so that it only puts stuff with the given IRI prefix
      (doall
       (clojure.core/filter
        (partial (or filter default-filter) owlontology)
        (.getSignature owlontology)))))
    owlontology))

(defn iri-create
  "DEPRECATED: Use iri method in tawny.owl.

A convenience method for creating IRIs.

Most namespaces that call use 'defread' will need to create an IRI.
This convenience method avoids the need for importing and depending
directly on the OWL API."
  {:deprecated "0.12"}
  [iri]
  (tawny.owl/iri iri))

(defmacro defread
  "Like read, but interns the ontology in symbol."
  [symbol & rest]
  `(do
    (def ~symbol
      (tawny.read/read ~@rest))))
