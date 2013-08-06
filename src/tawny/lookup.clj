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

(ns
    ^{:doc "Facilities to find the var of an OWL object."
      :author "Phillip Lord"}
    tawny.lookup
  (:require [tawny.owl]))

(defn- iri-for-var
  "Return the IRI for var if it is a named object
or nil otherwise."
  [var]
  (if (tawny.owl/named-object? (var-get var))
    (.toString (.getIRI (tawny.owl/as-named-object (var-get var))))
    nil))

(defn- vars-in-namespace
  "Return all the vars in the given namespace."
  [namespace]
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
  "Given a var, return a string representation of its name including the name
space, if it the var is not in the current namespace."
  [var]
  (let [ns (:ns (meta var))]
    (if (= ns *ns*)
      (var-str var)
      (var-qualified-str var))))

(defn named-entity-as-string
  "Given an OWLNamedObject, return the IRI."
  [entity]
  (-> entity
      (.getIRI)
      (.toString)))

(declare all-iri-to-var)
(defn resolve-entity
  "Given an OWLObject return a string representation of the var holding that
object. The string will be qualified if the var is not in the current
namespace."
  ([entity]
     (resolve-entity entity (all-iri-to-var)))
  ([entity iri-to-var]
      (let [entity-str (named-entity-as-string entity)
            var (get iri-to-var entity-str)]
        (if (nil? var)
          nil
          (var-maybe-qualified-str var)))))

(defn name-to-var
  "Returns a map of IRI for OWLObjects to the vars which
hold them for the given namespaces."
  [& namespaces]
  (into {}
        (for [[k v] (apply iri-to-var namespaces)]
          [(var-str v) v])))

(defn namespace-with-ontology
  "Returns a list of all names spaces with ontology objects"
  []
  (keys @tawny.owl/ontology-for-namespace))


;; This is an expensive operation, so we might want to cache it. However, we
;; cannot do this sensibly, because we don't have hooks for when new vars are
;; being created. So, instead, put all-iri-to-var-cache in a binding form for
;; the duration of the time that you wish the cache to last. Inside that
;; binding form, all-iri-to-var will just return the cache
(def
  ^{:dynamic true
    :doc "Holds a copy of the iri-to-var map
for the duration of the form."}
  all-iri-to-var-cache nil)


(defn all-iri-to-var
  "Returns a map keyed on the IRI of an OWLObject to var that holds that
OWLObject for all namespaces in which tawny has created an ontology."
  []
  (or all-iri-to-var-cache
      (do
        ;;#spy/d ^{:fs 10 :nses #"tawny"} ["calc"]
        (apply iri-to-var (namespace-with-ontology)))))
