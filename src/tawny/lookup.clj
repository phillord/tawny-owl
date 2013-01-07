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


(ns tawny.lookup
  (:require [tawny.owl]))

;; map between IRIs and axioms
;; these functions all operate on each other, and are broken out just to make
;; things easier to debug. 
(defn- iri-to-var [var]
  (if (tawny.owl/named-object? (var-get var))
    (.getIRI (tawny.owl/as-named-object (var-get var)))
    nil))

(defn- all-vars-in-namespace-with-ontology []
  (vals
   (apply
    merge
    (map
     (fn [x]
       (ns-publics x))
     (keys @tawny.owl/ontology-for-namespace)))))

(defn- pairs-iri-to-var []
  (for [k (all-vars-in-namespace-with-ontology)]
    [(iri-to-var k) k]))

(defn- filtered-iri-to-vars []
  (filter
   (comp not nil? first)
   (pairs-iri-to-var)))

(defn map-iri-to-var []
  (apply hash-map (flatten (filtered-iri-to-vars))))
