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


(ns tawny.repl
  (:require [tawny.owl :as o]
            [tawny.lookup]
            [tawny.render]
            [robert.hooke]
            [clojure.repl]
            ))



;; do a documentation formatter first
;; want to have two entry points -- "update all documentation" which does 
;; everything in a namespace. And everything for a single symbol, which 
;; I can hook into the macros. 

;; this does the job of adding metadata to an existing symbol. 
;; (intern *ns* (vary-meta 'test-without-doc assoc :doc "Now we have documentation"))


(defn fetch-doc 
  ([owlobject]
     (fetch-doc owlobject (o/get-current-ontology)))
  ([owlobject ontology]
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

           
           buffer (StringBuffer.)
           line (fn [& args]
                   (.append buffer 
                            (str (apply str args) "\n")))]

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
       
       
       ;;(line "Full Definition:")
       ;;       (str
       ;; (tawny.render/as-form owlobject))

       (.toString buffer))))


(defn print-doc
  ([owlobject]
     (println (fetch-doc owlobject)))

  ([owlobject ontology]
     (println (fetch-doc owlobject ontology))))

(defn print-doc-hook-function 
  [f m]
  (f 
   (if (:owl m)
     (assoc m 
       :doc 
       (fetch-doc
        (var-get 
         (ns-resolve (:ns m) (:name m)))
        (o/get-current-ontology (:ns m))
        ))
     m)))

;; augment the existing print-doc with this function
(robert.hooke/add-hook #'clojure.repl/print-doc #'print-doc-hook-function)
