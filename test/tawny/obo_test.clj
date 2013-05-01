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

(ns tawny.obo-test
  (:require
   [tawny.owl :as o]
   [tawny.obo :as obo])
  (:use [clojure.test])
  (:import [java.io File]))

(defn gen-test-ontology []
  (o/ontology :iri "http://iri"))

(deftest obo-iri-generate-or-retrieve
  (is (obo/preiri?
       (obo/obo-iri-generate-or-retrieve
        "test" {} {}))))

(deftest obo-iri-generate
  (is (empty?
       (o/with-ontology (gen-test-ontology)
         (:name-to-iri-current
          (deref (tawny.owl/ontology-options))))))
  (is (= 1
         (o/with-ontology
           (gen-test-ontology)
           (obo/obo-iri-generate "bob")
           (count (:name-to-iri-current
                   (deref (tawny.owl/ontology-options))))))))

(deftest obo-read-write
  (is
   (let [read
         (obo/obo-read-map (clojure.java.io/resource "test-read-obo.props"))]
     (and (map? read)
          (= 3 (count read))
          (= "http://a" (get read "A")))))

  (is
   (do
     (obo/obo-save-map
      (File/createTempFile "obo_test" ".props")
      {"A" "http://a" "B" "http://b"})
     true))

  (is
   (let [file (File/createTempFile "obo_test" ".props")
         map {"A" "http://a" "B" "http://b"}]
     (obo/obo-save-map file map)
     (= map
        (obo/obo-read-map file)))))


(deftest preiri?
  (is (obo/preiri? "http://purl.org/ontolink/preiri/fdsasfd"))
  (is (not (obo/preiri? "http://purl.org/ontolink/pizza"))))

(deftest obsolete
  (is
   (= {"A" "http://a"}
      (obo/extract-obsolete
       {"A" "http://a"}
       {})))

  (is
   (= {"A" "http://a"}
      (obo/extract-obsolete
       {"A" "http://a"
        "Temp" (str obo/obo-pre-iri "hello")}
       {}))))

(deftest update-map-with-new-iri
  (is
   (= {"A" "http://prefix_000001"}
      (obo/update-map-with-new-iri
       {"A" (str obo/obo-pre-iri "fda")}
       "http://prefix_")))
  (is
   (= {"A" "http://prefix_000010"
       "B" "http://prefix_000011"}
      (obo/update-map-with-new-iri
       {"A" "http://prefix_000010"
        "B" (str obo/obo-pre-iri "fda")}
       "http://prefix_")))

  )
