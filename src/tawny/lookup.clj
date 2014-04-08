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
  (:require [tawny owl util]))

(defn- iri-for-var
  "Return the IRI for var if it is a named object
or nil otherwise."
  [var]
  (when (tawny.owl/named-object? (var-get var))
    (str (.getIRI (tawny.owl/as-named-object (var-get var))))))

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
      tawny.owl/as-named-object
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
        (when-not (nil? var)
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


;; This is an expensive operation, so we cache it!
(def
  ^{:dynamic true
    :private true
    :doc "Holds a copy of the iri-to-var map
for the duration of the form."}
  all-iri-to-var-cache (atom nil))

(defn all-iri-to-var
  "Returns a map keyed on the IRI of an OWLObject to var that holds that
OWLObject for all namespaces in which tawny has created an ontology."
  []
  (if @all-iri-to-var-cache
    @all-iri-to-var-cache
    (reset! all-iri-to-var-cache
            (apply iri-to-var (namespace-with-ontology)))))

;; this actually gives us the var -- we could build things up as we go?
;; although this means we also have to cope with uninterns and the like as
;; they come. ultimately we are duplicating elsewhere?
(defn- kill-iri-cache
  "Dump the iri cache each time we add a new owl entity."
  [var]
  ;;  (println "blitzing iri-cache")
  (reset! all-iri-to-var-cache nil))

;; call back hook added -- we have to do this as a hook as it avoids
;; owl referencing lookup.
(tawny.util/add-hook
 tawny.owl/intern-owl-entity-hook
 #'kill-iri-cache)
