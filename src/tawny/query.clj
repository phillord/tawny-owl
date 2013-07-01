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
(ns tawny.query
  (:use [tawny.render])
  (:require [tawny.util]))

(def
  ^{:doc "Map between a form entity and a keyword"
    :private true}
  typemap
  {'defclass :class
   'owlclass :class
   'defoproperty :oproperty
   'object-property :oproperty
   'defindividual :individual
   'individual :individual
   'defdproperty :dproperty
   'data-property :dproperty
   'defannotationproperty :aproperty
   'annotation-property :aproperty
   }
  )

(defn- as-data-impl [strategy owlobject]
  (let [render
        (binding
            [terminal-strategy strategy
             tawny.lookup/all-iri-to-var-cache
             (tawny.lookup/all-iri-to-var)]
          (as-form owlobject))]
    (merge (tawny.util/hashify
            (drop 2 render))
           {:type (get typemap (first render))})))

(def as-data
  (partial as-data-impl :object))

(def as-data-recurse
  (partial as-data-impl (atom #{})))
