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

(def obo-pre-uri
  "http://purl.org/ontolink/preiri/")

(defn obo-iri-generate [name]
  (let [options (deref (tawny.owl/ontology-options))
        remembered
        (get options :name-to-iri-remembered {})
        current
        (get options :name-to-iri-current {})]
    (let [iri (or (get remembered name)
                  (get current name)
                  (str obo-pre-uri "#"
                       (java.util.UUID/randomUUID)))]
      (dosync
       (alter (tawny.owl/ontology-options)
              assoc
              :name-to-iri-current
              (assoc current name iri)))
      (IRI/create iri))))

(defn remember
  [& list]
  (apply hash-map list))

;; pull everything from  file
(defn obo-restore-iri [file]
  (let [name-to-iri-map
        (load-file file)]
    (dosync
     (alter (tawny.owl/ontology-options)
            merge {:name-to-iri-remembered name-to-iri-map}))))

;; store everything to a file
(defn obo-store-iri
  [file]
  (let [options (deref (tawny.owl/ontology-options))
        remembered (:name-to-iri-remembered options)
        ;; Remove from the remembered keys any that begin
        ;; with the obo-pre-uri. The point with this is that we should have
        ;; used them again anyway, so they will be in the current. If not, we
        ;; should forget about them, because they were temporary anyway.
        remembered-filtered
        (select-keys remembered
                     (for [[name iri] remembered
                           :when (not (.startsWith iri obo-pre-uri))]
                       name))]
    (with-open [w (clojure.java.io/writer file)]
      (.write w "(tawny.obo/remember\n")
      (doseq
          ;; the problem with this approach is that it's only going to work
          ;; with interned things. If we have a no interned entity, then we
          ;; have no way of getting the string back, since we have lost it
          ;; by now. This means that it won't work with (owlclass "a") style
          ;; calls.
          [[name iri] (merge
                       (:name-to-iri-current options)
                       remembered-filtered)]
        (.write w (format "\"%s\" \"%s\""
                          name
                          iri))
        (.newLine w))
      (.write w ")"))))


(defn obo-report-obsolete []
  (let [options (deref (tawny.owl/ontology-options))
        remembered (:name-to-iri-remembered options)
        remembered-filtered
        (select-keys remembered
                     (for [[name iri] remembered
                           :when (not (.startsWith iri obo-pre-uri))]
                       name))
        difference
        (clojure.set/difference remembered-filtered
                                (:name-to-iri-current options))]
    (doseq [[name iri] difference]
      (printf "Remembered but not longer in ontology: %s,%s"
              name iri))))

;; given a file, replace all the preiri's with real ones, at the repl, asking
;; as we go
(defn obo-generate-iri [filename])
