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


(declare form)

(defmulti as-form class)

(defmethod as-form AxiomedEntity [e]
  (as-form (:entity e)))

(defmethod as-form OWLClass [c]
  (let [super (.getSuperClasses c (owl/get-current-jontology))
        equiv (.getEquivalentClasses c (owl/get-current-jontology))
        disjoint (.getDisjointClasses c (owl/get-current-jontology))]
    `(owl.owl/defclass ~(form c)
       ~@(when (< 0 (count super))
           (cons
            :subclass
            (form super)))
       ~@(when (< 0 (count equiv))
           (cons
            :equivalent
            (form equiv)))
       ~@(when (< 0 (count disjoint))
           (cons
            :disjoint
            (form disjoint))))))
       
(defmulti form class)

(defmethod form Set [s]
  (map #(form %) s))

(defmethod form OWLClass [c]
  (.getFragment (.getIRI c)))

(defmethod form OWLObjectProperty [p]
  (.getFragment (.getIRI p)))

(defmethod form OWLObjectSomeValuesFrom [s]
  (list 'owlsome
        (form (.getProperty s))
        (form (.getFiller s))))

(defmethod form OWLObjectUnionOf [u]
  (list 'owlor (form (.getOperands u))))

(defmethod form OWLObjectIntersectionOf [c]
  (list 'owland (form (.getOperands c))))

(defmethod form OWLObjectAllValuesFrom [a]
  (list 'owlall
        (form (.getProperty a))
        (form (.getFiller a))))

(defmethod form OWLObjectComplementOf [c]
  (list 'owlnot
        (form (.getOperand c))))

(defmethod form OWLObjectExactCardinality [c]
  (list 'exactly (.getCardinality c)
        (form (.getProperty c))
        (form (.getFiller c))))


(defmethod form OWLObjectMaxCardinality [c]
  (list 'atmost (.getCardinality c)
        (form (.getProperty c))
        (form (.getFiller c))))

(defmethod form OWLObjectMinCardinality [c]
  (list 'atleast (.getCardinalty c)
        (form (.getProperty c))
        (form (.getFiller c))))


;; OWLObjectHasSelf
;; OWLObjectHasValue
;; OWLObjectOneOf





(defmethod form :default [e]
  `(not-sure-what-to-do))


(defmulti as-text class)

(defmethod as-text AxiomedEntity [e]
  (as-text (:entity e)))

(declare text)
(defmethod as-text OWLClass [c]
  (str (text c)
       "\n\t:subclass\n"
       (text (.getSuperClasses c (owl/get-current-jontology)))
       "\n\t:equivalent\n"
       (text (.getEquivalentClasses c (owl/get-current-jontology)))
       "\n\t:disjoint\n"
       (text (.getDisjointClasses c (owl/get-current-jontology)))))

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
           