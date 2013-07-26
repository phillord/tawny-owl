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

(ns tawny.fixture
  (:use [tawny.owl])
  (:require [tawny.reasoner :as r]))

(defn ontology-and-reasoner
  "Returns a fixture which sets o as the current ontology
and defines the reasoner factory to use."
  [o reasoner]
  (fn [tests]
    (r/reasoner-factory reasoner)
    (ontology-to-namespace o)
    (binding [r/*reasoner-progress-monitor*
              (atom r/reasoner-progress-monitor-silent)]
      (tests))))


(defn namespace-and-reasoner
  "Returns a fixture which sets the ontology from the namespace ns and defines
the reasoner factory to use. ns should be a symbol"
  [ns reasoner]
  (let [o (get @ontology-for-namespace (find-ns ns))]
    (fn [tests]
      (r/reasoner-factory reasoner)
      (ontology-to-namespace o)
      (binding [r/*reasoner-progress-monitor*
                (atom r/reasoner-progress-monitor-silent)]
        (tests)))))
