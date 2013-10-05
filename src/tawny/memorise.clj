;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Phillip Lord, Newcastle University

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

(ns tawny.memorise
  (:require [tawny.owl]
            [tawny.lookup]
            [clojure.set]))


(defn- change-values-to-string-set [map]
  (into {}
        (for [[k v] map]
          [k #{(tawny.lookup/var-str v)}])))


(defn- find-missing-mappings
  "Find mappings in old that are not present in current.

Both current and old are maps of IRI to set of string names"
  [current old]
  (into {}
        (filter (comp not empty? second)
                (merge-with
                 clojure.set/difference old current))))

(defn merge-with-distinct [x y]
  (into (sorted-map) (merge-with clojure.set/union x y)))


(defn memorise-map
  "Returns a map of entities to remember.

This function returns a map from the IRI to the var object(s) which hold it"
  ([]
     (memorise-map *ns*))
  ([namespace]
     (into {} (for [[k v] (tawny.lookup/iri-to-var namespace)] [k v]))))


(defn generate-obsolete-mapping
  [old-mappings mapping]
  (doall
   (map 
    (fn [x]
      (println "Generating mapping to obsolete symbol:" x " to " mapping)
      (intern *ns*
              (symbol x) 
              (fn []
                (println x " has changed its symbol please use " 
                         mapping " instead")
                (var-get mapping))))
    old-mappings)))



(defn fetch-remembered-entities []
  (or  
   (:remember (deref (tawny.owl/ontology-options)))
   {}))


(defn fetch-old-and-current-entities []
  (merge-with-distinct
    (change-values-to-string-set (memorise-map))
    (fetch-remembered-entities)))

(defn- check-old-mappings 
  "Check old mappings that we have remembered against those that we would
memorise now. Check that old mappings are still in memorise, otherwise create
a new mapping.

remember is a map iri to a set of names
memorise is a map iri to current var

"
  [remember memorise]

  (let 
      ;; memorise is iri to var, so need to get this as a string
      [memorise-iri-to-str
       (change-values-to-string-set memorise)
       ;; find the mappings that we are missing
       missing-mappings
       (find-missing-mappings memorise-iri-to-str remember)
       ]

    ;; generate new symbols for everything
    (doseq [[iri old-symbol-string] missing-mappings]

      (generate-obsolete-mapping
       old-symbol-string (get memorise iri)))))



(defn- memory-merge
"Accepts key and values as pairs, but preserves duplicate values for the
same keys, as all values are represented as sets"
  [items]
  (reduce 
   (partial merge-with 
            clojure.set/union)
   (map #(hash-map (first %) (hash-set (second %)))
        (partition 2 items))))


(defn memory 
  "Returns a map of IRI to set of string names for that concept"
  [& list]
  (memory-merge list))

(defn memorise
   "Save current memorise information in file."
   [file]
   (with-open [w (clojure.java.io/writer file)]
     (.write w "(tawny.memorise/memory\n")
     (doseq [[k v] (fetch-old-and-current-entities)]
       (doseq [name v]
         (.write w (format "\"%s\" \"%s\"\n" k name))))
     (.write w "\n)")))
     
(defn remember
  "Restore the current memorise information from file"
  [file]
   (let [iri-to-name-mapping
         ;; think this is going to be very dependant on the current directory.
         ;; So it's not going to work if another project imports the OBI
         ;; namespace.
         
         ;; the file contains a single "memory" form.
         (load-file file)]
     ;; store everything that we load, because we will need to save out
     ;; everything we load.
     (dosync
      (alter
       (tawny.owl/ontology-options)
       merge {:remember iri-to-name-mapping}))
     (check-old-mappings iri-to-name-mapping (memorise-map))))

