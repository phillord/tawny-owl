;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, 2014, Newcastle University

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

(ns tawny.pattern
  (:require [tawny.owl :as o]
            [tawny.util :as u]))


(o/defontology pattern
  :iri "http://example.com")

(o/defaproperty facetvalue
  :label "facet value"
  :comment "facet value indicates a relationship between an object property
  and a class, indicating that the class is a value in a particular facet
  where entities may be described by (one or more) existential relationship
  involving a specific object property. The object property may also have a
  range which is a superclass of the facet values but is not required to.")

(defn- nil-strip
  "Given a frame map with keyword keys and list values, remove any element in
  the list value which is nil, and both the key and value where all the
  elements are nil. This is useful for silently dropping nil calls to the
  frame functions in tawny.owl."
  [frames]
  (apply concat
         (for [[k v] (partition 2 frames)
               :when (some identity v)]
           (list* k (filter identity v)))))

(defn- nil-strip-hashify
  "Hashify args and call nil-strip. This is useful for silently dropping nil
  calls to most of the frame functions in tawny.owl."
  [args]
  (nil-strip
   (u/groupify args)))

(defn- nil-strip-hashify-op
  "Hashify args and call nil strip, suitable for the arguments to
  tawny.owl/object-property. This function uses adjacent keywords for
  characteristics."
  [args]
  (nil-strip
   (u/groupify-at
    (keys (var-get #'tawny.owl/object-property-handlers))
    args)))

(defrecord Named [name entity]
  tawny.owl.Entityable
  (as-entity [this] entity))

(defn p
  "Call the frame function entity-f but remove any nil arguments and the
  entire frames which only have nil arguments. Returns a vector of the name
  and the entity created, a form suitable for feeding to intern-owl-entities
  is necessary."
 [entity-f o name & options]
  (let
      [options
       (cond
        (= entity-f o/object-property)
        (nil-strip-hashify-op options)
        :default
        (nil-strip-hashify options))]
    (Named. name (apply entity-f o name options))))

(defn intern-owl-entities
  "Given a list of vectors of form [name entity], as returned by the p
  function, intern all the entities in the current namespace."
  [entities]
  ;; Interning works by side effect!
  (doall
   (map
    (fn [{:keys [name entity]}]
      (o/intern-owl-string name entity))
    entities)))

(o/defdontfn as-facet [o oprop & entities]
  "Mark entities as facet values for the facet oprop.
This allows the specification of a set of properties as classes without having
to explicitly name the object property."
  (doall
   (map
    (fn [e]
      (o/add-annotation
       o
       (o/as-entity (#'o/var-get-maybe e))
       (o/annotation o facetvalue
                     (o/as-iri
                      (o/as-entity oprop)))))
    (flatten entities))))

(defn- facet-property [o cls]
  ;; check for number!
  (first
   (map
    #(.getValue %)
    (filter
     #(= (.getProperty %)
         facetvalue)
     (.getAnnotations cls o)))))

(defn- facet-1 [o clazz]
  (o/owl-some
   o
   (facet-property o clazz)
   clazz))

(o/defdontfn facet
  "Return an existential restriction for each of the facetted classes."
  [o & clazz]
  (doall (map (partial facet-1 o) (flatten clazz))))

(defn extract-ontology-arg
  "Give a set of frame arguments, return a map with the ontology frame and all
  the other arguments without the ontology frame."
  [args]
  (let [[b a]
        (split-with (complement (fn [x] (= :ontology x))) args)
        ontology (second a)]
    {:ontology ontology
     :args
     (concat b (drop 2 a))}))

(defn pattern-generator
  "Macro generator function. Returns a form where args has the ontology frame
removed and placed as the first arg, which is passed to the pattern-function,
whose return values are interned."
  [pattern-function args]
  (let [ontsplit (extract-ontology-arg args)
        ont (:ontology ontsplit)
        rest (:args ontsplit)
        args-with-ont
        (if ont
          (cons ont rest)
          rest)]
    `(#'tawny.pattern/intern-owl-entities
      (~pattern-function ~@args-with-ont))))

(o/defmontfn value-partition
  "Return the entities for a new value partition.
partition-name is the overall name for the partition.
partition-values is a sequence of the values.
Keyword arguments are :comment for a comment to attach to
all entities.
:super the superclass of the partition.
:domain is the domain for the object property.

This returns a list of entity vectors created by the p function."
  [o partition-name partition-values
   & {:keys [comment super domain]}]
  (let
      [
       partition
       (p o/owl-class
          o partition-name
          :comment comment
          :super super)
       values
       (map
        #(p o/owl-class o
            %
            :comment comment
            :super partition)
        partition-values)
       prop
       (p o/object-property
          o  (str "has" partition-name)
          :characteristic :functional
          :comment comment
          :range partition
          :domain domain)]
    ;; we don't care about the return of this.
    (as-facet o prop values)
    (o/as-subclasses
     o partition
     :disjoint :cover
     values)
    ;; not to painful a return type -- would be nice to do this automatically,
    ;; perhaps from a modified let form, although this requires knowledge that
    ;; props is a list
    (list* partition prop values)))

(defmacro defpartition
  "As value-partition but accepts symbols instead of string and
takes the ontology as a frame rather than first argument."
  [partition-name partition-values & options]
  (tawny.pattern/pattern-generator
   'tawny.pattern/value-partition
   (list* (name partition-name)
          `(tawny.util/quote-word ~@partition-values)
          options)))
