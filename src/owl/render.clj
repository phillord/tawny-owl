;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, Newcastle University

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



(ns owl.render
  (:require [owl.owl :as owl])
  (:import (owl.owl AxiomedEntity)
           (java.util Set)
           (org.semanticweb.owlapi.model OWLClass
                                         OWLDataAllValuesFrom
                                         OWLDataExactCardinality
                                         OWLDataHasValue
                                         OWLDataMaxCardinality
                                         OWLDataMinCardinality
                                         OWLDataSomeValuesFrom
                                         OWLObjectAllValuesFrom
                                         OWLObjectComplementOf
                                         OWLObjectExactCardinality
                                         OWLObjectHasSelf
                                         OWLObjectHasValue
                                         OWLObjectIntersectionOf
                                         OWLObjectMaxCardinality
                                         OWLObjectMinCardinality
                                         OWLObjectOneOf
                                         OWLObjectSomeValuesFrom
                                         OWLObjectUnionOf

                                         OWLObjectProperty
                                         )))


(defmulti as-text class)

(defmethod as-text AxiomedEntity [e]
  (as-text (:entity e)))

(defmethod as-text OWLClass [c]
  (str (text c)
       "\n\t:subclass"
       (text (.getSuperClasses c (owl/get-current-jontology)))))

(defmulti text class)

(defmethod text AxiomedEntity [e]
  (text (:entity e)))

(defmethod text Set [s]
  (clojure.string/join
   "\n"
   (doall
    (map #(text %) s))))

(defmethod text OWLClass [c]
  (.getFragment (.getIRI c)))

(defmethod text OWLObjectProperty [p]
  (.getFragment (.getIRI p)))

(defmethod text OWLObjectSomeValuesFrom [s]
  (str "(some "
       (text (.getProperty s)) " "
       (text  (.getFiller s)) ")"))

(defmethod text OWLObjectUnionOf [u]
  (str "(or " (text (.getOperands u)) ")"))



(defmethod text :default [e]
  (str "Not sure how to handle:" e))

;; OWLDataAllValuesFrom
;; OWLDataExactCardinality
;; OWLDataHasValue
;; OWLDataMaxCardinality
;; OWLDataMinCardinality
;; OWLDataSomeValuesFrom
;; OWLObjectAllValuesFrom
;; OWLObjectComplementOf
;; OWLObjectExactCardinality
;; OWLObjectHasSelf
;; OWLObjectHasValue
;; OWLObjectIntersectionOf
;; OWLObjectMaxCardinality
;; OWLObjectMinCardinality
;; OWLObjectOneOf
;; OWLObjectUnionOf
           