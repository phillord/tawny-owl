;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Phillip Lord, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns tawny.memorise
  (:require [tawny.owl]))


;; this code is almost identical to lookup
(defn- iri-to-var [var]
  
  (if (tawny.owl/named-object? (var-get var))
    (.toString (.getIRI (tawny.owl/as-named-object (var-get var))))
    nil))

(defn- pairs-iri-to-var [namespace]
  (for  [k (vals  (ns-publics namespace))]
    [(iri-to-var k) k]))

(defn- filtered-iri-to-vars [namespace]
  (filter
   (comp not nil? first)
   (pairs-iri-to-var namespace)))

(defn map-iri-to-var [namespace]
  (apply hash-map (flatten (filtered-iri-to-vars namespace))))



(defn memorise-map
  "Returns a map of entities to remember.

This function returns a map from the IRI to the mapping that is currently
being used."
  ([]
     (memorise-map *ns*))
  ([namespace]
     (into {} (for [[k v] (map-iri-to-var namespace)] [k v]))))


(declare fetch-remembered-entities)
(defn memorise
   "Save current memorise information in file."
   [file]
   (with-open [w (clojure.java.io/writer file)]
     (.write w "(tawny.memorise/memory\n")
     (doseq [[k v] (fetch-old-and-current-entities)]
       (doseq [name v]
         (println name)
         (println k)
         ;;(println name " " k)
         (.write w (format "\"%s\" \"%s\"\n" k name ))))
     (.write w "\n)")))
     


(defn fetch-current-entities []
  (into {}
        (for [[k v] (memorise-map)]
          [k [(str (:name (meta v)))]])))

(defn fetch-old-and-current-entities []
  ;; sort so the print order is defined
  (sorted-map 
   (merge-with (comp distinct concat)
               (fetch-current-entities)
               (fetch-remembered-entities))))


(declare check-old-mappings)
(defn remember
  "Restore the current memorise information from file"
  [file]
   (let [iri-to-name-mapping
         ;; think this is going to be very dependant on the current directory.
         ;; So it's not going to work if another project imports the OBI
         ;; namespace.
         (load-file file)]
     (dosync
      (alter
       (:options (tawny.owl/get-current-ontology))
       merge {:remember iri-to-name-mapping}))
     (check-old-mappings iri-to-name-mapping (memorise-map))))


(defn fetch-remembered-entities []
  (:remember (deref (:options (tawny.owl/get-current-ontology)))))

(defn generate-obsolete-mapping
  [old-mappings mapping]
  (doall
   (map 
    (fn [x]
      (println "Generating mapping to obsolete symbol:" x " to " mapping)
      (intern *ns*
              (symbol  x) 
              (fn []
                (println x " has changed its symbol please use " 
                         mapping " instead")
                (var-get mapping))))
    old-mappings)))


(defn- check-old-mappings 
  "Check old mappings that we have remembered against those that we would
memorise now. Check that old mappings are still in memorise, otherwise create
a new mapping."
  [remember memorise]
  (doseq [[iri mapping] memorise]
    (let [remember-mappings (get remember iri)]
      (when (seq remember-mappings)
        (let [old-mappings
              (doall 
               (remove
                #(= (str (:name (meta mapping))) %) remember-mappings))]
          (when (seq old-mappings)
            (generate-obsolete-mapping old-mappings mapping)))))))


(defn- memory-merge
  ([list]
     (memory-merge list {}))
  ([remaining hash]
     (if (seq remaining)
       (recur 
        (rest (rest remaining))
        (merge-with concat
                    (hash-map
                     (first remaining)
                     (list (second remaining)))
                    hash))
       hash)))

(defn memory [& list]
  (memory-merge list))

