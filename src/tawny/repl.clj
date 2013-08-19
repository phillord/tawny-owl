;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

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


(ns ^{:author "Phillip Lord"
      :doc "Repl and documentation functions for OWL objects"}
    tawny.repl
  (:require [tawny.owl :as o]
            [tawny.lookup]
            [tawny.render]
            [clojure.pprint]
            )
  (:import [java.io StringWriter PrintWriter]))

(defn fetch-doc
  "Given an owlobject and potentially ontology, return documentation.
The documentation is generated over the live object (owlobjects are mutable).
It includes all labels, comments and a rendered version of the owlobject."
  ([owlobject]
     (fetch-doc owlobject (o/get-current-ontology)))
  ([owlobject ontology]
     (binding [tawny.lookup/all-iri-to-var-cache
                (tawny.lookup/all-iri-to-var)]
       (let [annotation (.getAnnotations owlobject ontology)
             label
             (filter
              #(-> %
                   (.getProperty)
                   (.isLabel))
              annotation)

             comment
             (filter
              #(-> %
                   (.getProperty)
                   (.isComment))
              annotation)

             iri (-> owlobject
                     (.getIRI)
                     (.toURI)
                     (.toString))

             writer (StringWriter.)
             pwriter (PrintWriter. writer)
             line (fn [& args]
                    (.println pwriter
                              (str (apply str args))))]

         (line "")

         (line

          (.toString (.getEntityType owlobject))
          ": "
          (tawny.lookup/var-maybe-qualified-str
           (get
            (tawny.lookup/all-iri-to-var) iri)))

         (line "IRI: " iri)
         (line "Labels:")
         (doseq [l label]
           (line "\t" (.getValue l)))

         (line "Comments:")
         (doseq [c comment]
           (line "\t" (.getValue c)))
         (line "Full Definition:")
         (o/with-ontology ontology
           (clojure.pprint/pprint
            (tawny.render/as-form owlobject)
            writer))

         (.toString writer)))))

(defn print-doc
  "Print the documentation for the owlobject. See fetch-doc for more on how
this documentation is generated."
  ([owlobject]
     (println (fetch-doc owlobject)))
  ([owlobject ontology]
     (println (fetch-doc owlobject ontology))))

(defn print-ns-doc
  "Print the documentation for all owlobjects stored in vars within a given
namespace."
  ([]
     (print-ns-doc *ns*))
  ([ns]
     (binding [tawny.lookup/all-iri-to-var-cache
                (tawny.lookup/all-iri-to-var)]
       (doseq [v
               (vals
                (tawny.lookup/iri-to-var ns))]
         (println (fetch-doc (var-get v)))))))


(defn update-var-doc
  "Updates the documentation on a var containing a OWLObject. This updates the
documentation based on the state of the OWLObject at the current time. See
also update-ns-doc which is more efficient for updating multiple vars at
once."
  [var]
  (alter-meta!
   var
   (fn [meta var]
     (assoc meta
       :doc (fetch-doc
             (var-get var))))
   var))

(defmacro update-doc
  "Updates the documentation on a symbol containing an OWLObject."
  [name]
  `(update-var-doc (var ~name)))

(defn update-ns-doc
  "Updates the documentation for all vars in a namespace."
  ([]
     (update-ns-doc *ns*))
  ([ns]
     (binding [tawny.lookup/all-iri-to-var-cache
                (tawny.lookup/all-iri-to-var)]
       (doseq [v
               (vals
                (tawny.lookup/iri-to-var ns))]
         (update-var-doc v)))))


(defn println-load-listener
  "Returns a OWLOntologyLoaderListener that logs to println."
  []
  (proxy [org.semanticweb.owlapi.model.OWLOntologyLoaderListener] []
    (finishedLoadingOntology[event]
      (println "Finished Loading:"
               (-> event
                   (.getOntologyID)
                   (.getOntologyIRI))
               (if (.isSuccessful event)
                 "...succeeded"))
      (if-not (.isSuccessful event)
        (println "Exception" (.printStackTrace (.getException event)))))
    (startedLoadingOntology [event]
      (println "Started Loading:"
               (-> event
                   (.getOntologyID)
                   (.getOntologyIRI))
               " from:"
               (-> event
                   (.getDocumentIRI))))))

(defn load-ontology
  "Loads and returns an ontology directly through the OWL API.
This is a useful test function; see 'tawny.read' for more integrated
  solution."
  [document-iri]
  (let [listener
        (println-load-listener)]
    (.addOntologyLoaderListener tawny.owl/owl-ontology-manager listener)
    (try
      (.loadOntologyFromOntologyDocument
       tawny.owl/owl-ontology-manager
       (tawny.owl/iri document-iri))
      (finally
        (.removeOntologyLoaderListener
         tawny.owl/owl-ontology-manager listener)))))
