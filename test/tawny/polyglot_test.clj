;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University

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

(ns tawny.polyglot-test
  (:use [clojure.test])
  (:require
   [tawny.fixture :as f]
   [tawny.owl :as o]
   [tawny.polyglot :as p]))

(def to nil)

(use-fixtures :each
  (f/test-ontology-fixture-generator #'to true)
  f/error-on-default-ontology-fixture)

(deftest polyglot-read-label
  (is
   (thrown? IllegalStateException
    (p/polyglot-load-label "abdjsk" "en"))))

(deftest load-a-b
  (is
   (do
     (o/owl-class to "A")
     (o/owl-class to "B")
     (p/polyglot-load-label "a-b-trans.props" "en")
     true)))
