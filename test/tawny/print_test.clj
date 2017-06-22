;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2017, Phillip Lord, Newcastle University
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

(ns tawny.print-test
  (:require
   [tawny.owl :as o]
   [tawny.print :as p]
   [clojure.test :refer :all]
   [tawny.fixture :as f]))


(def to nil)

(use-fixtures :each
  (f/test-ontology-fixture-generator #'to)
  f/error-on-default-ontology-fixture)

(deftest render-ontology
  (is
   (string?
    (pr-str to))))
