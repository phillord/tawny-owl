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

(ns tawny.lookup
  (:require [tawny.owl]))

(defn- iri-for-var
  "Return the IRI for var if it is a named object
or nil otherwise."
  [var]
  (if (tawny.owl/named-object? (var-get var))
    (.toString (.toURI (.getIRI (tawny.owl/as-named-object (var-get var)))))
    nil))

(defn- vars-in-namespace [namespace]
  (vals (ns-publics namespace)))

(defn iri-to-var 
  "Return a map of IRI to var for namespaces"
  [& namespaces]
  (into {}
        (for [var
              ;; flatten down to single list
              (flatten  
               ;; kill namespaces with no ontology terms
               (filter (comp not nil?)
                       ;; list per namespace
                       (map vars-in-namespace namespaces)))
              :let [iri (iri-for-var var)]
              :when (not (nil? iri))]
          [iri var])))


(defn var-str 
  "Given a var, return a string representation of its name"
  [var]
  (str (:name (meta var))))

(defn var-qualified-str
  "Given a var, return a string representation of its name
including the name space."
  [var]
  (str (:ns (meta var)) "/"
       (var-str var)))

(defn var-maybe-qualified-str
  "Given a var, return a string representation of its name
including the name space."
  [var]
  (let [ns (:ns (meta var))]
    (if (= ns *ns*)
      (var-str var)
      (var-qualified-str var))))


(defn name-to-var [& namespaces]
  (into {} 
        (for [[k v] (apply iri-to-var namespaces)]
          [(var-str v) v])))

(defn namespace-with-ontology
  "Returns a list of all names spaces with ontology objects"
  []
  (keys @tawny.owl/ontology-for-namespace))

(defn all-iri-to-var 
  "Returns a map keyed on IRI to var"
  []
  (apply iri-to-var (namespace-with-ontology)))


