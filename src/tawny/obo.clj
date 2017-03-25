;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University
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

;; You should have received a copy of the GNU Lesser General Public License;
; along with this program. If not, see http://www.gnu.org/licenses/.

(ns
    ^{:doc "Support for numeric, incrementing identifiers, OBO style."
      :author "Phillip Lord"}
    tawny.obo
  (:use [tawny.owl])
  (:require [tawny.lookup] [clojure edn pprint])
  (:import (org.semanticweb.owlapi.model IRI)))


(def
  ^{:doc "Root of IRI that is used for temporary IRIs"
    :tag 'String}
  obo-pre-iri
  "http://purl.org/ontolink/preiri/")

(defn obo-iri-generate-or-retrieve
  "Given an OWLEntity name return either the remembered name if there is one,
or the current, or generate a new temporary name"
  [name remembered current]
  (or (get remembered name)
      (get current name)
      (str obo-pre-iri "#"
           (java.util.UUID/randomUUID))))

(defdontfn obo-iri-generate
  "Generator function for numeric style IRIs. New OWLEntities will be given
an temporary ID, while existing OWLEntities will reuse a numeric, incrementing
ID. For full usage details see numeric.md documentation.

Implemented using ontology-options functionality. Newly created IDs are stored
in :name-to-iri-current, while IDs loaded from file are stored in
:name-to-iri-remembered."
  [o name]
  (let [options (deref (tawny.owl/ontology-options o))
        current
        (get options :name-to-iri-current {})
        iri (obo-iri-generate-or-retrieve
             name (get options :name-to-iri-remembered {})
             current)]
    (dosync
     (alter (tawny.owl/ontology-options o)
            assoc
            :name-to-iri-current
            (assoc current name iri)))
    (tawny.owl/iri iri)))

(defn obo-read-map
  "Read a properties file, and return a hashmap of Clojure identifier to IRI."
  [file]
  (with-open [r
              (java.io.PushbackReader.
               (clojure.java.io/reader file))]
    (apply hash-map
           (clojure.edn/read r))))

;; pull everything from file
(defdontfn obo-restore-iri
  "Read an existing properties file containing identifier to IRI data."
  [o file]
  (let [name-to-iri-map
        (obo-read-map file)]
    (dosync
     (alter (tawny.owl/ontology-options o)
            merge {:name-to-iri-remembered name-to-iri-map}))))

(defn preiri?
  "Return true if the (string) IRI is a auto-generated 'pre' IRI."
  [^String iri]
  (.startsWith iri obo-pre-iri))

(defn obo-sort
  "Sort identifiers. This is just to give a defined and repeatable order to
the save file, and is not functionality important for other reasons. Final
IRIs are placed before pre-IRIs, and both are organised alphabetically."
  [map]
  (sort-by second
           (fn [x y]
             (cond
              ;; put "real" iris first.
              (and (preiri? x)
                   (not (preiri? y)))
              false
              (and (not (preiri? x))
                   (preiri? y))
              true
              ;; both of the same type, organise alphabetically
              :default
              (compare x y)
              ))
           map))

(defn obo-save-map
  "Save a map into a properties file. Assumes entities are easily stringifiable."
  [file map]
  (with-open [w (clojure.java.io/writer file)]
    (clojure.pprint/pprint (flatten (obo-sort map)) w)))

;; store everything to a file
(defdontfn obo-store-iri
  "Save both existing and new identifier to IRI mappings into the given file."
  [o file]
  (let [options (deref (tawny.owl/ontology-options o))
        remembered (:name-to-iri-remembered options)
        ;; Remove from the remembered keys any that begin
        ;; with the obo-pre-iri. The point with this is that we should have
        ;; used them again anyway, so they will be in the current. If not, we
        ;; should forget about them, because they were temporary anyway.
        remembered-filtered
        (select-keys remembered
                     (for [[name iri] remembered
                           :when (not (preiri? iri))]
                       name))]
    (obo-save-map file
                  (merge
                   (:name-to-iri-current options)
                   remembered-filtered))))

(defn extract-obsolete
  "Extract identifiers for which there are mapping to a full IRI, but which no
longer exist in the ontology. There are, effectively, obsolete terms."
  [remembered current]
  (let
      ;; Fetch the only the remebered keys that do not start with the
      ;; temporary prefix. They will not be remembered next time we save
      ;; anyway.
      [remembered-filtered
       (select-keys remembered
                     (for [[name iri] remembered
                           :when (not (preiri? iri))]
                       name))]
    (apply dissoc remembered-filtered (keys current))))

(defdontfn obo-report-obsolete
  "Print a list of obsolete terms"
  [o]
  (let [options (deref (tawny.owl/ontology-options o))]
    (doseq [[name iri]
            (extract-obsolete
             (:name-to-iri-remembered options)
             (:name-to-iri-current options))
            ]
      (printf "Remembered but not longer in ontology: %s,%s\n"
              name iri))))


(defn update-map-with-new-iri
  "Assigns new permanent identifiers to map. In detail, this expects an map
between identifiers and IRI. For all those IRIs which return true for preiri?,
a new numeric identifier is created, incrementing from the current largest."
  [name-to-iri ^String prefix]
  (let ;; fetch the numeric part of IDs beginning with the prefix.
      [ids
       (map (fn [[name ^String iri]]
              (if (.startsWith iri prefix)
                (Integer/parseInt (.substring iri (.length prefix)))
                0))
            (seq name-to-iri))
        ;; the highest number of the current IDs
        biggest-numeric (apply max ids)
        ;; now get all the names that need new IDs
        preirinames
        (for [[name iri] name-to-iri
              :when (preiri? iri)]
          name)]
    ;; and finally update the IDs
    (merge
     name-to-iri
     (apply hash-map (interleave preirinames
                                 (map #(format "%s%06d" prefix %)
                                      (range (inc biggest-numeric)
                                             Integer/MAX_VALUE)))))))

;; given a file, replace all the preiri's with real ones, at the repl, asking
;; as we go
(defn obo-generate-permanent-iri
  "Given a file, replace all the preiris with permanent, incrementing, numeric
IDs."
  [file prefix]
  ;; and finally update the IDs
  (obo-save-map
   file
   (update-map-with-new-iri (obo-read-map file) prefix)))
