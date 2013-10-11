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
(ns tawny.memorise-test
  (:use [clojure.test])
  (:require [tawny.memorise :as m]
            [tawny.owl :as o]))


(defn test-memorise-map []
  (m/memorise-map *ns*))


(defn bind-some-vars []
  (eval
   '(do
      (tawny.owl/defclass a)
      (tawny.owl/defclass b)
      (tawny.owl/defclass c))))

(defn- unbind-some-vars []
  (ns-unmap *ns* 'a)
  (ns-unmap *ns* 'b)
  (ns-unmap *ns* 'c))

(deftest bind-and-unbind
  (is
   (= 0 (count
         (do (o/ontology)
             (test-memorise-map)))))

  (is
   (= 3
      (do (o/ontology)
          (bind-some-vars)
          (let [x
                (count
                 (test-memorise-map))]
            (unbind-some-vars)
            x))))

  (is
   (= 0 (count
         (do (o/ontology)
             (bind-some-vars)
             (unbind-some-vars)
             (test-memorise-map)))))

  (is
   (= 0
      (do
        (o/ontology)
        (count (test-memorise-map))))))


(deftest memorise-map []
  ;; this is a crappy test... need to change vars to string as we can't
  ;; reference the vars statically here. change-values-to-string-set is a way
  ;; of doing this.
   (=
    {"http://iri/#a" #{"a"},
     "http://iri/#b" #{"b"},
     "http://iri/#c" #{"c"}}
    (do (o/ontology)
        (bind-some-vars)
        (let [x
              (#'m/change-values-to-string-set (test-memorise-map))]
          (unbind-some-vars)
          x))))


(deftest memory-merge []
  (is
   (= {:a #{:b} :c #{:d} :e #{:f}}
      (#'m/memory-merge '(:a :b :c :d :e :f))
      ))

  (is
   (= {:a #{:e :b}, :c #{:f :d} :g #{:h}}
      (#'m/memory-merge '(:a :b, :c :d, :a :e, :c :f, :g :h))
      )))


(deftest find-missing-mappings
  (is
   (= {}
      (#'m/find-missing-mappings
       {"iri1" #{"symbol1"}}
       {"iri1" #{"symbol1"}})))

  (is
   (= {}
      (#'m/find-missing-mappings
       {"iri1" #{"symbol1" "symbol2"}}
       {"iri1" #{"symbol1"}})))

  (is
   (= {"iri1" #{"symbol2"}}
      (#'m/find-missing-mappings
       {"iri1" #{"symbol1"}}
       {"iri1" #{"symbol1" "symbol2"}})))

  )


(deftest memory
  (is
   (= {"a" #{"1"}}
      (m/memory "a" "1")
      ))
  (is
   (= {"a" #{"1" "2"}}
      (m/memory "a" "1" "a" "2")
      )))


(deftest change-values-to-string-set
  (is
   (= {
       "http://iri/#b" #{"b"},
       "http://iri/#c" #{"c"},
       "http://iri/#a" #{"a"},
       }
      (do
        (bind-some-vars)
        (let [retn
              (#'m/change-values-to-string-set (m/memorise-map *ns*))]
          (unbind-some-vars)
          retn))))

  (is
   (= {}
      (#'m/change-values-to-string-set (m/memorise-map *ns*)))))



(deftest merge-with-distinct
  (is
   (= {"a" #{"1"}}
      (#'m/merge-with-distinct
        {} {"a" #{"1"}})))

  (is
   (= {"a" #{"1"}}
      (#'m/merge-with-distinct
        {"a" #{"1"}} {})))


  (is
   (= {"a" #{"1"}}
      (#'m/merge-with-distinct
        {"a" #{"1"}} {"a" #{"1"}})))


  (is
   (= {"a" #{"1" "2"}}
      (#'m/merge-with-distinct
        {"a" #{"1"}} {"a" #{"1" "2"}}))))
