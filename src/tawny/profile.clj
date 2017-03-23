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

(ns
    ^{:author "Phillip Lord"
      :doc "Check profile of OWL ontologies"}
    tawny.profile
  (:use [tawny.owl])
  (:import
   [org.semanticweb.owlapi.model OWLOntology]
   [org.semanticweb.owlapi.profiles OWL2ELProfile OWL2DLProfile
    OWLProfile OWL2Profile OWL2QLProfile
    OWL2RLProfile OWLProfileViolation]))

(def profile-owl2
  (OWL2Profile.))

(def profile-owl2dl
  (OWL2DLProfile.))

(def profile-owl2el
  (OWL2ELProfile.))

(defno violations
  "Return a list of violations of the given profile,
for an ontology or the current ontology."
  [^OWLOntology ontology ^OWLProfile profile]
  (.getViolations (.checkOntology profile ontology)))

(defno inprofile?
  "Returns true if an ontology or the current ontology is
in the current profile"
  [^OWLOntology ontology ^OWLProfile profile]
  (.isInProfile (.checkOntology profile ontology)))

(defno axiom-violations
  "Return a list of axioms that are violation of the given profile,
in an ontology or the current ontology"
  [ontology profile]
  (map #(.getAxiom ^OWLProfileViolation %) (violations ontology profile)))
