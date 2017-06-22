;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University

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
  (:require [tawny owl util protocol type]))

(declare all-iri-to-var)

(defn iri-to-var
  "Given `namespaces` return an iri to var map for
all vars containing an OWLObject in these namespaces."
  [& namespaces]
  (into
   {}
   (for [[iri vr] (all-iri-to-var)
         :when (some
                #{(:ns (meta vr))}
                namespaces)]
     [iri vr])))

(defn iri-to-var-no-ontology [& namespaces]
  (into
   {}
   (for [[iri vr]
         (apply iri-to-var namespaces)
         :when
         (not
          (tawny.type/ontology?
           (var-get vr)))]
     [iri vr])))

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
      tawny.owl/as-iriable
      tawny.protocol/as-iri
      (.toString)))

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
  ^{:private true
    :doc "Cached copy of the IRI to var map. Automatically deleted on reload."}
  all-iri-to-var-cache (atom nil))

(defn- var-interned? [var]
  (let [{:keys [ns name]} (meta var)]
    (ns-resolve ns name)))

(defn all-iri-to-var
  "Returns a map keyed on the IRI of an OWLObject to var that has been
  created for any OWLObject. This method can return a vars which are
  no longer interned, perhaps by ns-unmap. Use `clean-var-cache` to
  clear these out."
  []
  @all-iri-to-var-cache)

(defn clean-var-cache []
  (swap! all-iri-to-var-cache
         #(into {}
                (filter
                 (fn [[k v]]
                   (var-interned? v)))
                %)))

(defn- cache-var-map [var]
  (swap! all-iri-to-var-cache
         assoc
         (named-entity-as-string (var-get var))
         var))

;; call back hook added -- we have to do this as a hook as it avoids
;; owl referencing lookup.
(tawny.util/add-hook
 tawny.owl/intern-owl-entity-hook
 #'cache-var-map)
