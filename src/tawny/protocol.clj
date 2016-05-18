;; #+TITLE: tawny.protocols: Protocols for Tawny
;; #+AUTHOR: Phillip Lord

;; * Header :no-export


;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2016, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See they
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns tawny.protocol)

;; ** Entityable

;; Where ever possible, I try to pass around OWL API objects and reuse their
;; functionality directly. The original motivation for this was just pragmatic;
;; Tawny was incomplete and I needed to fallback to OWL API calls often.
;; Unfortunately, the OWL API does not support all of the functionality I need
;; within Tawny, so I have worked around this with wrapping objects, usually
;; Records. This seemed simpler than alternative methods, such as a ~WeakHashMap~
;; store, or something similar to the ~ontology-options~ support added later.

;; We extend to ~Object~ and ~nil~ primarily for datatypes, but also for ~String~
;; based usage of Tawny.

;; #+begin_src clojure
(defprotocol Entityable
  "Entityable objects are objects which wrap an OWLObject for some purpose."
  (^OWLEntity as-entity [entity]
    "Returns the bare OWLObject from an Entityable entity."))

;; extend to object so that we can support numbers and strings, and so forth.
(extend-type
    Object
  Entityable
  (as-entity [entity] entity))

(extend-type
    nil
  Entityable
  (as-entity [entity] nil))
;; #+end_src


;; ** Annotatable

;; ~Annotatable~ supports annotations of entities that need to be placed on the
;; ~OWLAxiom~ that is used to assert its relationship with other things.
;; The frame based syntax of tawny means that these annotations are implicit in
;; the syntax (although not in the underlying code base) which makes them a bit
;; of a pain to implement. We use a wrapping object as the most practical
;; alternative here.

;; The annotations are applied at the point that the relationship is asserted,
;; not the point that the ~annotate~ function is called, so the same annotation
;; could be applied to many axioms.

;; #+begin_src clojure
(defprotocol Annotatable
  "Annotatable objects are objects which can contain a number of OWLAnnotations."
  (^java.util.Set as-annotations [entity]
    "Returns an OWLAnnotation set from an Annotatable entity."))

(extend-type
    Object
  Annotatable
  (as-annotations [entity]
    #{}))


;; ** IRIable

;; Within Tawny, we need to convert a number of things to IRI objects. We offer
;; basic support for this conversion, and a protocol. The OWL API also offers an
;; ~HasIRI~ interface (added during the tawny implementation), but I also want to
;; extend this to Clojure types, hence the protocol.



;; for reasons that I do not understand IRI has to be fully-qualified or
;; lookup.clj gives errors. I can't reproduce this with a simple test case.
(defprotocol IRIable
  (^org.semanticweb.owlapi.model.IRI as-iri [entity]
    "Returns an IRI identifying the entity if this is possible"))
