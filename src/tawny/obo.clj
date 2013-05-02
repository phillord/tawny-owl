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

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns tawny.obo
  (:use [tawny.owl])
  (:require [tawny.lookup] [clojure.set])
  (:import (org.semanticweb.owlapi.model IRI
            )))

(def obo-pre-iri
  "http://purl.org/ontolink/preiri/")

(defn obo-iri-generate-or-retrieve
  [name remembered current]
  (or (get remembered name)
      (get current name)
      (str obo-pre-iri "#"
           (java.util.UUID/randomUUID))))

(defn obo-iri-generate [name]
  (let [options (deref (tawny.owl/ontology-options))
        current
        (get options :name-to-iri-current {})
        iri (obo-iri-generate-or-retrieve
             name (get options :name-to-iri-remembered {})
             current)]
    (dosync
     (alter (tawny.owl/ontology-options)
            assoc
            :name-to-iri-current
            (assoc current name iri)))
    (IRI/create iri)))

(defn obo-read-map [file]
  (with-open [r (clojure.java.io/reader file)]
    (apply hash-map
           (flatten
            (for [line (line-seq r)
                  :let [[name iri] (clojure.string/split line #"=")]]
              [name iri])))))

;; pull everything from file
(defn obo-restore-iri [file]
  (let [name-to-iri-map
        (obo-read-map file)]
    (dosync
     (alter (tawny.owl/ontology-options)
            merge {:name-to-iri-remembered name-to-iri-map}))))

(defn preiri?
  [iri]
  (.startsWith iri obo-pre-iri))

(defn obo-sort [map]
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
  [file map]
  (with-open [w (clojure.java.io/writer file)]
    ;; we could probably do with sorting this.

    (doseq
        [[name iri]
         (obo-sort map)]
      (.write w (format "%s=%s"
                        name
                        iri))
      (.newLine w))))

;; store everything to a file
(defn obo-store-iri
  [file]
  (let [options (deref (tawny.owl/ontology-options))
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

(defn extract-obsolete [remembered current]
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

(defn obo-report-obsolete []
  (let [options (deref (tawny.owl/ontology-options))]
    (doseq [[name iri]
            (extract-obsolete
             (:name-to-iri-remembered options)
             (:name-to-iri-current options))
            ]
      (printf "Remembered but not longer in ontology: %s,%s\n"
              name iri))))


(defn update-map-with-new-iri [name-to-iri prefix]
  (let ;; fetch the numeric part of IDs beginning with the prefix.
      [ids
       (map (fn [[name iri]]
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
                                      (range (+ 1 biggest-numeric)
                                             Integer/MAX_VALUE)))))))

;; given a file, replace all the preiri's with real ones, at the repl, asking
;; as we go
(defn obo-generate-permanent-iri [file prefix]
  ;; and finally update the IDs
  (obo-save-map
   file
   (update-map-with-new-iri (obo-read-map file) prefix)))
