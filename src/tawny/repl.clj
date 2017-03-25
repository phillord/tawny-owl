;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, 2014, 2017, Newcastle University

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
            [tawny.protocol :as p]
            [tawny.render]
            [clojure.pprint]
            )
  (:import [java.io StringWriter PrintWriter]
           [org.semanticweb.owlapi.model
            OWLAnnotation
            OWLEntity
            OWLOntology
            OWLNamedObject
            OWLOntologyLoaderListener$LoadingEvent
            OWLOntologyLoaderListener$LoadingFinishedEvent
            IRI OWLOntologyManager
            ]
           ))

(o/defmontfn fetch-doc
  "Given an owlobject and potentially ontology, return documentation.
The documentation is generated over the live object (owlobjects are mutable).
It includes all labels, comments and a rendered version of the owlobject."
  ([^OWLOntology ontology ^OWLEntity owlobject]
     (let [annotation
           (org.semanticweb.owlapi.search.EntitySearcher/getAnnotations
            owlobject ontology)
           label
           (filter
            (fn [^OWLAnnotation a]
              (-> a
                   (.getProperty)
                   (.isLabel)))

            annotation)

           comment
           (filter
            (fn [^OWLAnnotation a]
              (-> a
                  (.getProperty)
                  (.isComment)))
            annotation)

           iri (-> owlobject
                   (.getIRI)
                   (.toURI)
                   (.toString))

           writer (StringWriter.)
           pwriter (PrintWriter. writer)
           line (fn [& args]
                  (.println pwriter
                            (str (clojure.string/join args))))]
       (line "")
       (line
        (str (.getEntityType owlobject))
        ": "
        (tawny.lookup/var-maybe-qualified-str
         (get
          (tawny.lookup/all-iri-to-var) iri)))

       (line "IRI: " iri)
       (line "Labels:")
       (doseq [^OWLAnnotation l label]
         (line "\t" (.getValue l)))

       (line "Comments:")

       (doseq [^OWLAnnotation c comment]
         (line "\t" (.getValue c)))
       (line "Full Definition:")
       (line
        ;; hmm pprint here takes 95% of the time. Problematic
        ;; str is much much quicker, but produces a rubbishy output!
        (clojure.pprint/pprint
         (tawny.render/as-form owlobject)
         writer))
       (.close writer)
       (str writer))))

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
     (doseq [v
             (vals
              (tawny.lookup/iri-to-var ns))]
       (println (fetch-doc (var-get v))))))


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
     (doseq [v
             (vals
              (tawny.lookup/iri-to-var ns))]
       (update-var-doc v))))


(defn println-load-listener
  "Returns a OWLOntologyLoaderListener that logs to println."
  []
  (proxy [org.semanticweb.owlapi.model.OWLOntologyLoaderListener] []
    (finishedLoadingOntology
      [^OWLOntologyLoaderListener$LoadingFinishedEvent event]
      (println "Finished Loading:"
               (-> event
                   (.getOntologyID)
                   (.getOntologyIRI))
               (if (.isSuccessful event)
                 "...succeeded"))
      (if-not (.isSuccessful event)
        (println "Exception" (.printStackTrace (.getException event)))))
    (startedLoadingOntology
      [^OWLOntologyLoaderListener$LoadingEvent event]
      (println "Started Loading:"
               (or (-> event
                       (.getOntologyID)
                       (.getOntologyIRI))
                   "unknown")
               " from:"
               (.getDocumentIRI event)))))


(defn ^OWLOntologyManager new-manager
  "Returns a new OWLOntologyManager."
  []
  (org.semanticweb.owlapi.apibinding.OWLManager/createOWLOntologyManager))

(defn load-ontology
  "Loads and returns an ontology directly through the OWL API.
This is function is meant for usage at the REPL; see 'tawny.read' for more
integrated solution. If an manager is passed in, it should not already have
loaded an ontology with the same name."
  ([^IRI iri ^OWLOntologyManager manager]
     (let [listener
           (println-load-listener)]
       (.addOntologyLoaderListener
        manager listener)
       (.loadOntologyFromOntologyDocument
        manager
        iri)))
  ([iri]
     (load-ontology iri (new-manager))))

(defn materialize-ontology
  "Loads an ontology, attempts to resolve all of its imports, then
saves the import clojure to the resources directory. Returns a map of IRI
to file names. Save ontologies in 'root' or the resources directory."
  ([iri]
     (materialize-ontology iri "dev-resources/"))
  ([iri root]
      (let [manager (new-manager)
            ontology (load-ontology iri manager)]
        (into {}
              (for [^OWLOntology k (.getOntologies manager)]
                [(-> k
                     (.getOntologyID)
                     (.getOntologyIRI)
                     (.toString))
                 (let
                     [file-maybe
                      (-> k
                          (.getOntologyID)
                          (.getOntologyIRI)
                          (.orNull)
                          (p/as-iri)
                          (.getFragment))
                      stem
                      (if file-maybe
                        file-maybe
                        (str (java.util.UUID/randomUUID)))
                      file (str root stem)]
                   (.saveOntology manager k
                                  (java.io.FileOutputStream.
                                   (java.io.File. file)))
                   stem)])))))

(def *c
  "The last change that an on-change listener saw."
  (atom nil))

(defn on-change
  "Evaluate F everytime an ontology change happens"
  [f]
  (let [listener
         (proxy [org.semanticweb.owlapi.model.OWLOntologyChangeListener]
             []
             (ontologiesChanged
              [l]
               (reset! *c l)
               (f)))]
     (.addOntologyChangeListener
      (o/owl-ontology-manager)
      listener)
     listener))

(def auto-save-listener
  "The current listener for handling auto-saves or nil." (atom nil))

(defn auto-save
  "Autosave the current ontology everytime any change happens."
  ([filename format]
     (auto-save filename format false))
  ([filename format nosave]
     (let [f #(o/save-ontology filename format)]
       ;; save immediately
       (when-not nosave
         (f))
       (when-not @auto-save-listener
         (reset! auto-save-listener
                 (on-change f))))))

(defn auto-save-off
  "Stop autosaving ontologies."
  []
  (when auto-save-listener
    (.removeOntologyChangeListener
     (o/owl-ontology-manager)
     auto-save-listener)))

(defn name-annotations
  ([]
     (name-annotations (o/get-current-ontology)))
  ([^org.semanticweb.owlapi.model.OWLOntology o]
     (filter
      (fn [axiom]
        (tawny.util/on-type
         org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom axiom
         (= (var-get #'tawny.owl/tawny-name-property)
            (.getProperty axiom))))
      (.getAxioms o))))

(defn make-no-name
  "Remove name annotations from current and future entities in the current
  ontology. This is not quite the same as setting :noname at the beginning,
  but it useful at the REPL where these annotations can just hinder the
  reading of the ontology. There is no way to reverse this function, so use
  with care."
  ([]
     (make-no-name (o/get-current-ontology)))
  ([o]
     (dosync
      (alter
       (tawny.owl/ontology-options o)
       merge {:noname true}))
     (apply tawny.owl/remove-axiom
            o (name-annotations))))

(defn render-ontology
  ([^OWLOntology o file]
     (render-ontology o file {}))
  ([^OWLOntology o file options]
     (println "Rendering:" o)
     (spit file "")
     (let [render-options
           (flatten
            (seq
             (merge
              {:explicit true
               :ontologies #{o}}
              options)))]
       (doseq [n (.getSignature o)]
         (spit
          file
          (str
           (pr-str
            (apply
             tawny.render/as-form
             n render-options)) "\n")
          :append true)))))

(defn render-iri
  [iri file]
  (println "Loading:" iri)
  (let [o (load-ontology iri)]
    (render-ontology o file)))

(defn use-tawny
  "Use key tawny namespaces.
This is designed for repl uses with lein-shorthand."
  []
  (use 'tawny.owl)
  (use 'tawny.repl)
  (use 'tawny.reasoner)
  (use 'tawny.render)
  (use 'tawny.pattern))
