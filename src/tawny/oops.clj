;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(ns ^{:doc "Deploy ontology to OOPS! RESTful Web Service"
      :author "Jennifer D. Warrender"}
  tawny.oops
  (:use [tawny.owl])
  (:require [clojure.data.xml :as xml]
            [clj-http.client :as client]
            [tawny.read :only [read]])
  (:import
   (java.io ByteArrayOutputStream ByteArrayInputStream)
   (org.semanticweb.owlapi.io RDFXMLOntologyFormat)))

;; GENERATION FUNCTIONS
(defn- ensure-generic
  ""
  [valid test string]
    (if (not (every? (partial contains? valid) test))
      (throw
       (IllegalArgumentException.
        (str string
             (clojure.string/join
              " "
              (filter #(not (contains? valid %)) test)))))))

(defn- ensure-valid-keys
  "Ensures that all KEYS are valid."
  [keys]
  (let [valid-keys #{:url :content :pitfall :outformat}]
    (ensure-generic valid-keys keys "Unknown key(s): ")))

(defn- ensure-mandatory
  "Ensures that INPUT contains some value."
  [input]
  (if (empty? (remove empty? input))
    (throw
     (IllegalArgumentException.
      "No ontology URL or ontology RDF text provided.")))
  (if (every? #(not (empty? %)) input)
    (throw
     (IllegalArgumentException.
      "Ontology URL and onology RDF text provided -- provide either
      value not both."))))

(defn- ensure-valid-pitfalls
  "Ensures that all PITFALLS are valid. Required as not all identified
  pitfalls have been implemented -- see http://oops-ws.oeg-upm.net/"
  [pitfalls]
  (if (not (vector? pitfalls))
    (throw
     (IllegalArgumentException.
      "Pitfall(s) input should be of type vector.")))
  (let [valid-pitfalls
        (into #{}
              (flatten
               (apply merge (range 2 14) (range 19 23) (range 24 30))))]
    (ensure-generic valid-pitfalls pitfalls "Invalid pitfall(s): ")))

(defn- ensure-valid-format
  "Ensures that output FORMAT is vaild."
  [format]
  (let [valid-formats #{"XML" "RDF/XML"}]
    (ensure-generic valid-formats [format] "Unknown output format: ")))

(defn- build-request
  "Builds the XML body of the request using ARGS. If <OntologyContent>
is provided then the RDF code must be contained inside CDATA
structure. If <Pitfalls> are provided they the numbers must be comma
seperated."
  [& args]
  (let [map (apply hash-map args)]
    (ensure-valid-keys (keys map))
    (ensure-mandatory [(:url map "") (:content map "")])
    (ensure-valid-pitfalls (:pitfall map []))
    (ensure-valid-format (:outformat map "RDF/XML"))
    (let [url (:url map "")
          content0 (:content map "")
          content (if (empty? content0) content0 (xml/cdata content0))
          pitfall0 (:pitfall map [])
          pitfall (if (empty? pitfall0) pitfall0
                      (clojure.string/join "," pitfall0))
          outformat (:outformat map "RDF/XML")]
      (xml/emit-str
       (xml/element :OOPSRequest {}
                    (xml/element :OntologyUrl {} url)
                    (xml/element :OntologyContent {} content)
                    (xml/element :Pitfalls {} pitfall)
                    (xml/element :OutputFormat {} outformat))))))

(defn- get-rdf-ontology
  "Returns the RDFXML format of the ontology O."
  [o]
  (let [output-stream (new ByteArrayOutputStream)]
    (.saveOntology
     (owl-ontology-manager)
     o
     (RDFXMLOntologyFormat.)
     output-stream)
    output-stream))

(def ^{:private true
       :doc "The OOPS! Web Service URL."}
  oops-post-url "http://oops-ws.oeg-upm.net/rest")

(defn- deploy
  "Returns OOPS ontology results of the HTTP POST with given BODY request."
  [body]
  (:body ;; test this
   (client/post oops-post-url {:body body})))

(defn oops-url
  "Builds and HTTP POSTs a request using the ontology's URL and ARGS."
  [url & args]
  (deploy
   (apply build-request
          (conj (into [] args) :url url))))

(defn oops-ontology
  "Builds and HTTP POSTs the O ontology request using ARGS."
  [o & args]
  (let [ont (.toString (get-rdf-ontology o) "UTF-8")]
    (deploy
     (apply build-request
          (conj (into [] args) :content ont)))))

(defn oops-file
  "Builds and HTTP POSTs a request using a local ontology document and
ARGS. FILENAME location is madatory, while ONTIRI and PREFIX are
optional."
  [filename ontiri prefix & args]
  (let [o (tawny.read/read
           :location
           (iri (clojure.java.io/as-file filename))
           :iri ontiri
           :prefix prefix)]
    (apply oops-ontology o args)))

(defn load-results-ontology
  "Using the RESULTS of out HTTP POST, we load the OOPS ontology
results."
  [results]
  (let [input-stream (new ByteArrayInputStream (.getBytes results "UTF-8"))]
    (tawny.read/read
     :location input-stream
     :iri "http://www.oeg-upm.net/oops#"
     :prefix "oops:")))

(defn load-results-file
  "Loads the OOPS ontology results from the local FILENAME file."
  [filename]
  (tawny.read/read
   :location
   (iri (clojure.java.io/as-file filename))
   :iri "http://www.oeg-upm.net/oops#"
   :prefix "oops:"))

(defn- annotation-filter
  "Returns the literal value of (the first) annotation axiom from
  ANNOTATIONS that uses the annotation PROPERTY."
  [annotations property]
  (.getLiteral
   (.getValue (first (filter #(= property (.getProperty %)) annotations)))))

(defn- get-oops-annotation-values
  "Returns an ordered list of literal values for a set of OOPS
ANNOTATIONS proerties."
  [o annotations]
  (let [oops-properties
        ["http://www.oeg-upm.net/oops#hasCode"
         "http://www.oeg-upm.net/oops#hasDescription"
         "http://www.oeg-upm.net/oops#hasImportanceLevel"
         "http://www.oeg-upm.net/oops#hasName"
         "http://www.oeg-upm.net/oops#hasNumberAffectedElements"]]
    (map #(annotation-filter annotations
                             (annotation-property o (iri %)))
         oops-properties)))

(defn get-oops-results
  "Returns an ordered list of oops pitfall results and their details
for an OOPS ontology O."
  [o]
  (let [instances
        (direct-instances
         o (owl-class o (iri "http://www.oeg-upm.net/oops#pitfall")))
        annotations
        (map #(.getAnnotationAssertionAxioms o (.getIRI %)) instances)]
    (sort-by first (map (partial get-oops-annotation-values o)
                        annotations))))