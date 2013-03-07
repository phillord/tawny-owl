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

(ns tawny.profile
  (:use [tawny.owl])
  (:import [org.semanticweb.owlapi.profiles OWL2ELProfile OWL2DLProfile
            OWL2Profile OWL2QLProfile OWL2RLProfile]))

(def profile-owl2
  (OWL2Profile.))

(def profile-owl2dl
  (OWL2DLProfile.))

(def profile-owl2el
  (OWL2ELProfile.))

(defontfn violations
  "Return a list of violations of the given profile, 
for an ontology or the current ontology."
  [ontology profile]
  (.getViolations (.checkOntology profile ontology)))

(defontfn inprofile?
  "Returns true if an ontology or the current ontology is
in the current profile"
  [ontology profile]
  (empty? (violations ontology profile)))

(defontfn axiom-violations
  "Return a list of axioms that are violation of the given profile,
in an ontology or the current ontology"
  [ontology profile]
  (map #(.getAxiom %) (violations ontology profile)))
