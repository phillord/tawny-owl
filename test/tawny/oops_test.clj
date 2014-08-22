;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2014, Newcastle University

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

(ns tawny.oops-test
 (:require [tawny.owl :as o]
           [tawny.oops :as oo]
           [clojure.data.xml :as xml])
  [:use clojure.test])

;; See http://nakkaya.com/2009/11/18/unit-testing-in-clojure/
(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(with-private-fns [tawny.oops [ensure-generic
                               ensure-valid-keys
                               ensure-mandatory
                               ensure-valid-pitfalls
                               ensure-valid-format
                               build-request
                               get-rdf-ontology
                               deploy]]

  (deftest ensure-generic
    (is
     (nil?
      (ensure-generic [1 2 3 4] [1 2] "Invalid: ")))
    (is
     (nil?
      (ensure-generic [1 2 3 4] [] "Invalid: ")))

    ;; failing test
    (is (thrown? IllegalArgumentException
                 (ensure-generic [] [1 2] "Invalid: "))))

  (deftest ensure-valid-keys
    (is
     (nil?
      (ensure-valid-keys [:url :content :pitfall :outformat])))

    ;; failing test
    (is (thrown? IllegalArgumentException
                 (ensure-valid-keys [:pitfalls]))))

  (deftest ensure-mandatory
    (is
     (nil?
      (ensure-mandatory ["http://" ""])))
    (is
     (nil?
      (ensure-mandatory ["" "TODO"])))

    ;; failing test
    (is (thrown? IllegalArgumentException
                 (ensure-valid-keys ["" ""])))
    (is (thrown? IllegalArgumentException
                 (ensure-valid-keys ["http://" "TODO"]))))

  (deftest ensure-valid-pitfalls
    (is
     (nil?
      (ensure-valid-pitfalls
       [2 3 4 5 6 7 8 10 11 12 13 19 20 21 22 24 25 25 26 27 28 29])))

    ;; failing test
    (is (thrown? IllegalArgumentException
                 (ensure-valid-pitfalls 1)))
    (is (thrown? IllegalArgumentException
                 (ensure-valid-pitfalls [1]))))

  (deftest ensure-valid-format
    (is
     (nil?
      (ensure-valid-format "XML")))
    (is
     (nil?
      (ensure-valid-format "RDF/XML")))

    ;; failing test
    (is (thrown? IllegalArgumentException
                 (ensure-valid-format "RDF"))))

  (deftest build-request
    (let [url "http://www.cc.uah.es/ie/ont/learning-resources.owl"
          content "TODO"
          format "XML"]

      (is
       (= (build-request :url url
                         :pitfall [10]
                         :outformat format)
          (xml/emit-str
           (xml/element :OOPSRequest {}
                        (xml/element :OntologyUrl {} url)
                        (xml/element :OntologyContent {} "")
                        (xml/element :Pitfalls {} 10)
                        (xml/element :OutputFormat {} format)))))

      (is
       (= (build-request :content content
                         :pitfall [10 11]
                         :outformat format)
          (xml/emit-str
           (xml/element :OOPSRequest {}
                        (xml/element :OntologyUrl {} "")
                        (xml/element :OntologyContent {} (xml/cdata content))
                        (xml/element :Pitfalls {} "10,11")
                        (xml/element :OutputFormat {} format)))))))

  (deftest get-rdf-ontology
    (let [to (o/ontology :name "to"
                         :iri "http://test"
                         :prefix "test:")
          rdf (get-rdf-ontology to)
          file (slurp (clojure.java.io/resource "to.owl"))]
      (is
       (= (.toString rdf) file))))

  (deftest deploy
    (is
     (not
      (nil?
       (re-find #"P10</oops:hasCode>"
                (deploy
                 (build-request
                  :url "http://www.cc.uah.es/ie/ont/learning-resources.owl"
                  :pitfall [10])))))))
)

(deftest oops-url
  (let [url "http://www.cc.uah.es/ie/ont/learning-resources.owl"
        pitfall [10]]
    (is
     (not
      (nil?
       (re-find #"P10</oops:hasCode>" (oo/oops-url url :pitfall pitfall)))))))

(deftest oops-ontology
  (let [to (o/ontology :name "to"
                       :iri "http://test"
                       :prefix "test:")]
    (o/owl-class to "A")
    (is
     (not
      (nil?
       (re-find #"P04</oops:hasCode>"
                (oo/oops-ontology to)))))))

(deftest oops-file
  (let [infile (clojure.java.io/resource "learning-resources.owl")
        iniri "http://www.cc.uah.es/ie/ont/learning-resources#"
        inprefix ""
        pitfall [10]]
    (is
     (not
      (nil?
       (re-find #"P10</oops:hasCode>"
                (oo/oops-file infile iniri inprefix :pitfall pitfall)))))))

(deftest load-results-ontology
  (let [url "http://www.cc.uah.es/ie/ont/learning-resources.owl"
        pitfall [10]
        results (oo/oops-url url :pitfall pitfall)
        o (oo/load-results-ontology results)]
    (is
     (= 14
        (count (.getSignature o))))
    (is
     (not
      (nil?
       (o/owl-class o (o/iri "http://www.oeg-upm.net/oops#pitfall")))))))

(deftest load-results-file
  (let [infile (clojure.java.io/resource "oops-response.owl")
        o (oo/load-results-file infile)]
    (is
     (= 14
        (count (.getSignature o))))
    (is
     (not
      (nil?
       (o/owl-class o (o/iri "http://www.oeg-upm.net/oops#pitfall")))))))

(with-private-fns [tawny.oops [annotation-filter
                               get-oops-annotation-values]]

  (deftest annotation-filter
    (let [to (o/ontology :name "to"
                         :iri "http://test"
                         :prefix "test:")
          property1 (o/annotation-property to "hasAnnotation1")
          property2 (o/annotation-property to "hasAnnotation2")
          annotations [(o/annotation to property1 (o/literal "test1"))
                       (o/annotation to property2 (o/literal "test2"))]]
      (is
       (= "test1"
          (annotation-filter annotations property1)))
      (is
       (= "test2"
          (annotation-filter annotations property2)))))

  (deftest get-oops-annotation-values
    (let [to (o/ontology :name "to"
                         :iri "http://test"
                         :prefix "test:")
          oops-properties
          ["http://www.oeg-upm.net/oops#hasCode"
           "http://www.oeg-upm.net/oops#hasDescription"
           "http://www.oeg-upm.net/oops#hasImportanceLevel"
           "http://www.oeg-upm.net/oops#hasName"
           "http://www.oeg-upm.net/oops#hasNumberAffectedElements"]
          annotation-properties
          (into [] (map #(o/annotation-property to (o/iri %))
                        (reverse oops-properties)))
          literals [1 "name" "level" "description" "code"]
          test-data
          (for [i (range 0 (count oops-properties))]
            (o/annotation to
                          (get annotation-properties i)
                          (o/literal
                           (get literals i))))]
      (is
       (=
        ;; OWLLiteral.getValue() always returns java.lang.String
        (reverse (map str literals))
        (get-oops-annotation-values to test-data)))))
)

(deftest get-oops-results
  (let [infile (clojure.java.io/resource "oops-response.owl")
        o (oo/load-results-file infile)]
    (is
     (= (oo/get-oops-results o)
        (list
         (list
          "P10"
          "The ontology lacks disjoint axioms between classes or between properties that should be defined as disjoint. For example, we can create the classes \"Odd\" and \"Even\" (or the classes \"Prime\" and \"Composite\") without being disjoint; such representation is not correct based on the definition of these types of numbers."
          "Important"
          "Missing disjointness [1, 2, 3]"
          "1"))))))