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
     (nil? @tawny.reasoner/vreasoner-factory)
     "No reasoner has been set"
     (nil? ns)
     "Namespace not recognised"
     (nil? (tawny.owl/get-current-ontology ns))
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
      (if (tawny.owl/with-ontology
            (tawny.owl/get-current-ontology ns)
            (tawny.reasoner/coherent?))
        "Ontology is coherent."
        "Ontology is not coherent."))))

(defn is-consistent
  "Returns a message about the consistency of
NAMESPACE"
  [namespace]
  (let [ns (check-namespace namespace)]
    (if (instance? String ns)
      ns
      (if (tawny.owl/with-ontology
            (tawny.owl/get-current-ontology ns)
            (tawny.reasoner/consistent?))
        "Ontology is consistent."
        "Ontology is not consistent."))))


(defn list-classes [classes]
  "Given a set of classes, returns a string representation."
  (clojure.string/join
   "\n"
   (map
    #(.toString %)
    classes)))

(defn get-unsatisfiable [namespace]
  "Returns a string about unsatisfiable classes."
  (let [ns (check-namespace namespace)]
    (if (instance? String ns)
      ns
      (if-let
          [unsatis
           (seq (tawny.owl/with-ontology
                  (tawny.owl/get-current-ontology ns)
                  (tawny.reasoner/unsatisfiable)))]
        (list-classes unsatis)
        "Ontology has no unsatisfiable classes."))))
