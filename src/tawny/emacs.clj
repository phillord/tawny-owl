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
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns
    ^{:doc "Support functions for tawny-mode.el"
      :author "Phillip Lord"}
    tawny.emacs
  [:require [tawny owl reasoner util]])

;; Executes commands and returns strings which emacs displays. Clunky but
;; effective.
(defn set-reasoner
  "Sets the reasoner factory."
  [reasoner]
  (when
      (tawny.reasoner/reasoner-factory reasoner)
    (format "Reasoner has been set: %s" reasoner)))


(defn check-namespace
  "Test namespace for error condition.

Returns error message or namespace if everything is fine."
  [namespace]
  (let [ns (find-ns (symbol namespace))]
    (cond
     (nil? @#'tawny.reasoner/vreasoner-factory)
     "No reasoner has been set"
     (nil? ns)
     "Namespace not recognised"
     (nil? (tawny.owl/get-current-ontology-maybe ns))
     "No ontology in namespace"
     :default
     ns)))

(defn is-coherent
  "Returns a message about the coherency of
NAMESPACE"
  [namespace]
  (let [ns (check-namespace namespace)]
    (if (instance? String ns)
      ns
      (if
        (tawny.reasoner/coherent?
         (tawny.owl/get-current-ontology ns))
        "Ontology is coherent."
        "Ontology is not coherent."))))

(defn is-consistent
  "Returns a message about the consistency of
NAMESPACE"
  [namespace]
  (let [ns (check-namespace namespace)]
    (if (instance? String ns)
      ns
      (if
          (tawny.reasoner/consistent? (tawny.owl/get-current-ontology ns))
        "Ontology is consistent."
        "Ontology is not consistent."))))

(defn list-classes
  "Given a set of classes, returns a string representation."
  [classes]
  (clojure.string/join
   "\n"
   (map
    #(str ^Object %)
    classes)))

(defn get-unsatisfiable
  "Returns a string about unsatisfiable classes."
  [namespace]
  (let [ns (check-namespace namespace)]
    (if (instance? String ns)
      ns
      (if-let
          [unsatis
           (seq (tawny.reasoner/unsatisfiable
                 (tawny.owl/get-current-ontology ns)))]
        (list-classes unsatis)
        "Ontology has no unsatisfiable classes."))))

(defn get-inferred-superclasses
  "Returns a string of inferred superclasses."
  [namespace class]
  (let [ns (check-namespace namespace)]
    (if-let [superclasses
             (seq (tawny.reasoner/isuperclasses
                   (tawny.owl/get-current-ontology ns)
                   (var-get (ns-resolve ns (symbol class)))))]
      (list-classes superclasses)
      "Ontology has no inferred superclasses.")))

(defn get-inferred-subclasses
  "Returns a string of inferred subclasses."
  [namespace class]
  (let [ns (check-namespace namespace)]
    (if-let [subclasses
             (seq (tawny.reasoner/isubclasses
                   (tawny.owl/get-current-ontology ns)
                   (var-get (ns-resolve ns (symbol class)))))]
      (list-classes subclasses)
      "Ontology has no inferred subclasses.")))

(defn save-namespace-ontology
  "Save ontology in the given namespace."
  [namespace]
  (let [o (get @tawny.owl/ontology-for-namespace
               (find-ns (symbol namespace)))]
    (tawny.owl/save-ontology
     o "o.omn" :omn)
    (tawny.owl/save-ontology
     o "o.owl" :owl)))
