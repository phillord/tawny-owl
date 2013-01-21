;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Newcastle University

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

(ns tawny.read_test
  (:refer-clojure :exclude [read])
  (:use [clojure.test])
  (:require [tawny.read :as r]
            [tawny.owl :as o])
  (:import (org.semanticweb.owlapi.model IRI OWLNamedObject))

  )


;; test the resource discovery is working!
(deftest wine-as-resource
  (is (clojure.java.io/resource "wine.rdf")))


(deftest read
  (is (r/read :location 
              (IRI/create (clojure.java.io/resource "wine.rdf"))
              :iri "http://iri"
              :viri "http://viri"
              :prefix "wine:"
              ))
  )



