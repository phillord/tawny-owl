;; #+TITLE: Pattern
;; #+AUTHOR: Phillip Lord

;; * Header

;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, 2014, 2015 Newcastle University

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


;; * Namespace

;; We start with a namespace declaration. As we will using a number of annotation
;; properties in this namespace, we define that here to.

;; #+begin_src clojure
(ns tawny.pattern
  (:require
   [clojure.set]
   [tawny.owl :as o]
   [tawny.protocol :as p]
   [tawny.util :as u])
  (:import [org.semanticweb.owlapi.model
            OWLAnnotation OWLAnnotationAxiom
            OWLAnnotationAssertionAxiom OWLEntity
            OWLOntology OWLClass OWLObject]
           [org.semanticweb.owlapi.search
            EntitySearcher]))

(o/defontology pattern
  :iri "http://www.w3id.org/ontolink/pattern.owl")
;; #+end_src


;; * Intering Macro Support

;; Writing functions is relatively straight-forward, but writing macros is a bit
;; more of a pain. So, here we add generic support for turning a function
;; defining a pattern into a macro. To do this, we essentially need three things.

;;  - the pattern needs to return OWL entities and a string to intern.
;;  - for macros the "ontology as the first argument" paradigm doesn't work, so
;;    we need support for adding an ontology frame and extracting it again.
;;  - something to actually do the interning.

;; To support the first, we introduce the ~Named~ record to package an entity
;; with the name to refer to it; the ~p~ function introduced later actually uses
;; these. The ~pattern-generator~ and ~extract-ontology-arg~ functions support
;; the latter. And ~intern-owl-entities~ supports the last.

;; Examples of these in use will be seen later.

;; #+begin_src clojure
(defrecord Named [name entity])

(extend-type
    Named
  tawny.protocol/Entityable (as-entity [this] (:entity this)))

(defn intern-owl-entities
  "Given a list of vectors of form [name entity], as returned by the p
  function, intern all the entities in the current namespace."
  [entities]
  ;; Interning works by side effect!
  (doall
   (map
    (fn [{:keys [name entity]}]
      (o/intern-owl-string name entity))
    entities)))

(defn extract-ontology-arg
  "Give a set of frame arguments, return a map with the ontology frame and all
  the other arguments without the ontology frame."
  [args]
  (let [[b a]
        (split-with (complement (fn [x] (= :ontology x))) args)
        ontology (second a)]
    {:ontology ontology
     :args
     (concat b (drop 2 a))}))

(defn pattern-generator
  "Macro generator function. Returns a form where args has the ontology frame
removed and placed as the first arg, which is passed to the pattern-function,
whose return values are interned."
  [pattern-function args]
  (let [ontsplit (extract-ontology-arg args)
        ont (:ontology ontsplit)
        rest (:args ontsplit)
        args-with-ont
        (if ont
          (cons ont rest)
          rest)]
    `(#'tawny.pattern/intern-owl-entities
      (~pattern-function ~@args-with-ont))))
;; #+end_src


;; * Easy Pattern Creation with "P"

;; The frame syntax of tawny makes it easy to type but harder to parse.
;; Generally, ~tawny.owl~ can take care of this, but there is a problem where
;; parts of the pattern are optional. For example, consider:

;; #+begin_example
;; (owl-class "newclass" :super s)
;; #+end_example

;; This is a pattern because we use the free variable ~s~. The problem comes if
;; ~s~ is ~nil~, which tawny treats as an error (for good reasons!).
;; Unfortunately, we cannot just strip nil values because we might have this:

;; #+begin_example
;; (owl-class "newclass" :super s :equivalent e)
;; #+end_example

;; If ~s~ is nil and we nil-strip arguments, then we end up with
;; ~:super :equivalent~ as a sequence, which tawny also treats as an error (for
;; good reasons!). So, provide ~p~ which nil-strip and removes frames with
;; nothing but nils. So:

;; #+BEGIN_EXAMPLE
;; (p owl-class "newclass" :super nil :equivalent e)
;; #+END_EXAMPLE

;; now works because both ~:super~ and ~nil~ are removed. Other uses such as:

;; #+BEGIN_EXAMPLE
;; (p owl-class "newclass" :super nil c :equivalent e)
;; #+END_EXAMPLE

;; are also fine. Of course, this puts the onus on the calling function to ensure
;; that only values which are meant to be optional are passed and are not nil.

;; ~p~ also modifies the return type. It extracts the first argument
;; (~"newclass"~ in the examples) and returns a ~Named~ entity for use with the
;; interning support.

;; #+begin_src clojure
(defn- nil-strip
  "Given a frame map with keyword keys and list values, remove any element in
  the list value which is nil, and both the key and value where all the
  elements are nil. This is useful for silently dropping nil calls to the
  frame functions in tawny.owl."
  [frames]
  (apply concat
         (for [[k v] (partition 2 frames)
               :when (some identity v)]
           (list* k (filter identity v)))))

(defn- nil-strip-hashify
  "Hashify args and call nil-strip. This is useful for silently dropping nil
  calls to most of the frame functions in tawny.owl."
  [args]
  (nil-strip
   (u/groupify args)))

(defn- nil-strip-hashify-op
  "Hashify args and call nil strip, suitable for the arguments to
  tawny.owl/object-property. This function uses adjacent keywords for
  characteristics."
  [args]
  (nil-strip
   (u/groupify-at
    (keys (var-get #'tawny.owl/object-property-handlers))
    args)))

(defn p
  "Call the frame function entity-f but remove any nil arguments and the
  entire frames which only have nil arguments. Returns a vector of the name
  and the entity created, a form suitable for feeding to intern-owl-entities
  is necessary."
  [entity-f o name & options]
  (let
      [options
       (cond
        (= entity-f o/object-property)
        (nil-strip-hashify-op options)
        :default
        (nil-strip-hashify options))]
    (Named. name (apply entity-f o name options))))
;; #+end_src

;; * Pattern Annotation

;; It is often useful to be able to group all of the entities created as a the
;; result of an instantiation of a pattern, so that it is possible to retrieve
;; all these entities. This section provides this capability.

;; The implementation has one important consequence; I have chosen to use
;; annotation properties, rather than representing this knowledge Clojure side
;; which would have been perfectly possible. This means that the use of patterns
;; are represented explicitly in the final ontology, as an annotation between the
;; class and the object property. This obviously has consequences for the final
;; serialized form of the ontology which may or may not be a good thing. On
;; balance, I think, representing the knowledge externally is positive, and not
;; having to store extra global state is definately good.


;; #+begin_src clojure
(o/defaproperty inpattern
  :label "In Pattern"
  :comment "Indicates that the entity was created as part of a pattern
with other entities that are annotated to the same anonymous individual.")

(defonce
  ^{:private true
    :doc "Cache for the anonymous individuals used to annotate patterns."}
  pattern-annotator-cache
  (atom {}))

(o/defmontfn pattern-annotator
  "Annotates all ENTITIES as part of a pattern.

  This associated each entity with an anonymous individual. The individual is
  cached based on NAME or the .toString of the first entity. This is an
  attempt to make the function relatively idempotent, rather than just adding
  a new annotation each time."
  ([o entities]
   (pattern-annotator o entities (str (first entities))))
  ([o entities name]
   (let [cache (get @pattern-annotator-cache
                    name)
         anon (if cache
                cache
                (let [anon
                      (.getOWLAnonymousIndividual (o/owl-data-factory))]
                  (swap! pattern-annotator-cache assoc name anon)
                  anon))]
     (doall
      (map
       #(o/refine
         o
         (p/as-entity %)
         :annotation
         (o/annotation inpattern anon))
       entities)))
   entities))

(o/defdontfn pattern-annotations
  "Returns pattern annotations of ENTITY or the empty list."
  [^OWLOntology o entity]
  (filter
   (fn [^OWLAnnotation anon]
     (= (.getProperty anon)
        inpattern))
   (EntitySearcher/getAnnotations
    (p/as-entity entity) o)))

(o/defdontfn which-pattern
  "Returns the OWLAnonymousIndividual(s) describing the pattern(s)
  which ENTITY is part of."
  [o entity]
  (map
   (fn [^OWLAnnotation anon] (.getValue anon))
   (pattern-annotations o entity)))

(o/defdontfn shared-pattern
  "Return the OWLAnonymousIndividual(s) describing the pattern(s)
to which all ENTITIES belong."
  [o & entities]
  (apply clojure.set/intersection
         (map
          #(set
            (which-pattern o %))
          entities)))

(o/defdontfn pattern-iris
  "Return all IRIs that are in pattern annotated with PATTERN,
an anonymous invididual."
  [^OWLOntology o pattern]
  (map
   (fn [^OWLAnnotationAssertionAxiom anon] (.getSubject anon))
   (filter
    (fn [^OWLAnnotationAssertionAxiom anon]
      (=
       (.getValue anon)
       pattern))
    (.getAxioms
     o
     org.semanticweb.owlapi.model.AxiomType/ANNOTATION_ASSERTION))))

(o/defdontfn pattern-entities
  "Return all entites that are in pattern annotated with PATTERN,
an anonymous invididual."
  [^OWLOntology o pattern]
  (map
   #(o/entity-for-iri o %)
   (pattern-iris o pattern)))
;; #+end_src


;; * Facets

;; This is our first example of a pattern. Many ontologies define entities by a
;; set of properties with particular values, rather like a childhood card game;
;; cars have speed, engine size, number of people and so on; planes have wing
;; span, passangers and altitude.

;; Here, we add support for these that we call *facets*; where a set of classes
;; are used as values for a specific property. The relationship between the class
;; and the propety is stored as a annotation, using the ~as-facet~ function,
;; while the ~facet~ function returns one or more existential restrictions using
;; the appropriate property (of which there may be several). This means that
;; multiple facets can be expressed rapidly just by using their values.


;; #+begin_src clojure
(o/defaproperty facetvalue
  :label "facet value"
  :comment "facet value indicates a relationship between an object property
  and a class, indicating that the class is a value in a particular facet
  where entities may be described by (one or more) existential relationship
  involving a specific object property. The object property may also have a
  range which is a superclass of the facet values but is not required to.")

(o/defdontfn as-facet [o oprop & entities]
  "Mark entities as facet values for the facet oprop.
This allows the specification of a set of properties as classes without having
to explicitly name the object property."
  (doall
   (map
    (fn [e]
      (o/add-annotation
       o
       (p/as-entity (#'o/var-get-maybe e))
       (o/annotation facetvalue
                     (p/as-iri
                      (p/as-entity oprop)))))
    (flatten entities))))

(defn- facet-property [^OWLOntology o
                       ^OWLClass cls]
  (let
      [prop
       (map
        (fn [^OWLAnnotation anon] (.getValue anon))
        (filter
         (fn [^OWLAnnotation anon]
           (= (.getProperty anon)
              facetvalue))
         (EntitySearcher/getAnnotations cls o)))]
    (if (= 1 (count prop))
      ;; there should be only one, and we have no good basis to pick if there
      ;; are more than one.
      (first prop)
      (throw (Exception.
              (str "There are multiple facet properties for class:" cls))))))

(defn- facet-1 [o clazz]
  (o/owl-some
   o
   (facet-property
    o
    (#'tawny.owl/ensure-class o clazz))
   clazz))

(o/defdontfn facet
  "Return an existential restriction for each of the facetted classes."
  [o & clazz]
  (doall (map (fn [c] (facet-1 o c)) (flatten clazz))))
;; #+end_src


;; * Value Partition

;; This is the value partition property that is used to split something
;; continuous into a set of discrete entities; rather like the colours of the
;; rainbow.

;; #+begin_src clojure
(o/defmontfn value-partition
  "Return the entities for a new value partition.

PARTITION-NAME is the overall name for the partition.

PARTITION-VALUES is a sequence of the values. The values can either be strings
representing the name of the value. Or a vector, the head of which is the
name, and all the other values are additional frames to be passed to
`tawny.owl/owl-class`.

Keyword arguments are :comment for a comment to attach to
all entities.
:super the superclass of the partition.
:domain is the domain for the object property.

This returns a list of entity vectors created by the p function."
  [o partition-name partition-values
   & {:keys [comment super domain]}]
  (let
      [
       partition
       (p o/owl-class
          o partition-name
          :comment comment
          :super super)
       values
       (map
        #(apply
          p (concat
             [o/owl-class o]
             (if (sequential? %)
               %
               (list %))
             [:comment comment
              :super partition]))
        partition-values)
       prop
       (p o/object-property
          o  (str "has" partition-name)
          :characteristic :functional
          :comment comment
          :range partition
          :domain domain)]
    ;; we don't care about the return of this.
    (as-facet o prop values)
    (o/as-subclasses
     o partition
     :disjoint :cover
     values)
    ;; not to painful a return type -- would be nice to do this automatically,
    ;; perhaps from a modified let form, although this requires knowledge that
    ;; props is a list
    (pattern-annotator
     o (list* partition prop values))))

(defmacro defpartition
  "As `value-partition` but accepts symbols instead of string and takes the
  ontology as a frame rather than first argument."
  [partition-name partition-values & options]
  (tawny.pattern/pattern-generator
   'tawny.pattern/value-partition
   (list* (name partition-name)
          `(tawny.util/quote-word-or-head ~@partition-values)
          options)))

(o/defmontfn partition-values
  "Given a value partition return the values.
O is the Ontology, P the value partition."
  [o p]
  (filter
   #(and
     (not (= p %))
     (instance? OWLClass %)
     (o/subclass? o p %))
   ;; We could just check all entities here, but that is hostage to users
   ;; making subclasses of values. Limiting to those in the same pattern
   ;; prevents this.
   (flatten
    (map
     (partial pattern-entities o)
     (which-pattern o p)))))
;; #+end_src


;; * Footer

;; Pattern.clj Ends Here
