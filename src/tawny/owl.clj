;; #+TITLE: tawny.owl: Building Ontologies Programmatically.
;; #+AUTHOR: Phillip Lord

;; * Header :no-export

;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, 2013, 2014, 2015, 2016, Newcastle University

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


;; * Namespace

;; #+begin_src clojure
(ns ^{:doc    "Build ontologies in OWL."
      :author "Phillip Lord"}
  tawny.owl
  (:require
   [clojure.walk :only postwalk]
   [clojure.test :only is]
   [clojure.set]
   [clojure.java.io]
   [tawny.print]
   [tawny.protocol :as p]
   [tawny.type :as t]
   [tawny.util :as util])
  (:import
   (org.semanticweb.owlapi.model
    HasIRI
    OWLAxiom
    OWLEntity
    OWLObject
    OWLOntologyManager OWLOntology IRI
    OWLClassExpression OWLClass OWLAnnotation
    OWLIndividual OWLDatatype
    OWLObjectPropertyExpression
    OWLNamedObject OWLOntologyID
    OWLAnnotationProperty OWLObjectProperty
    OWLDataFactory OWLDocumentFormat
    OWLDataProperty OWLDataRange OWLProperty OWLPropertyExpression
    OWLDataPropertyExpression OWLLiteral)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.semanticweb.owlapi.formats ManchesterSyntaxDocumentFormat
                                   TurtleDocumentFormat
                                   OWLXMLDocumentFormat
                                   RDFXMLDocumentFormat)
   (org.semanticweb.owlapi.util DefaultPrefixManager OWLEntityRemover)
   [org.semanticweb.owlapi.search
    EntitySearcher]
   (java.io ByteArrayOutputStream FileOutputStream PrintWriter)
   (java.io File)
   (java.util Collections)
   (org.semanticweb.owlapi.model AddAxiom RemoveAxiom AddImport
                                 AddOntologyAnnotation)))

;; #+end_src

;; * Assertions

;; We add type assertions to selected places because otherwise long
;; definitions can be hard to debug, particularly form individuals.

;; Unfortunately, clojure pre/post conditions report the form that has failed,
;; but not the value of these forms which makes them largely useless for
;; reporting errors.

;; The `clojure.test/is` macro provides a (slightly strange) way of adding this
;; reporting function. However, it requires only a single form. Using `and` fails
;; because it then shows only `false` on failure. So we create an macro here
;; which expands these out.

;; #+begin_src
(defmacro ^{:private true} say
  "Reporter macro for assertions."
  [& conditions]
  `(do
    ~@(map
       (fn [con#]
         `(clojure.test/is ~con#))
       conditions)))
;; #+end_src

;; * Begin resource section

;; The next set of forms all contain values that percolate across all the
;; namespaces that contain ontologies. Resetting all of these when ~owl.clj~ is
;; eval'd is a pain, hence they are all ~defonce~. Access is via a fn, because
;; this is more adapatable and also because I monkey patch them inside protege
;; for tawny-nrepl, since this requires a different factory/manager for each GUI
;; frame. If I find myself doing this often, I may need to think again about the
;; implementation. They used to be closures but this was a pain to type hint

;; #+begin_src clojure

(defonce
  ^{:doc "The OWLDataFactory used for Tawny."
    :private true}
  vowl-data-factory (atom nil))

(defn ^OWLDataFactory owl-data-factory
 "Returns the main factory for all other objects."
  []
  (when-not @vowl-data-factory
    (reset! vowl-data-factory
            (OWLManager/getOWLDataFactory)))
  @vowl-data-factory)

(defonce
  ^{:doc "The OWLOntologyManager used for Tawny."
    :private true}
  vowl-ontology-manager (atom nil))

(defn ^OWLOntologyManager owl-ontology-manager
  "The single OWLOntologyManager used by Tawny."
  []
  (when-not @vowl-ontology-manager
    (reset! vowl-ontology-manager
            (OWLManager/createOWLOntologyManager)))
  @vowl-ontology-manager)

(defonce
  ^{:doc "Map between namespaces and ontologies"}
  ontology-for-namespace (ref {}))

;; #+end_src

;; * Protocols and Predicates

;; #+begin_src clojure
(defn ^IRI iri
  "Returns an IRI object given a string or URI. Does no transformation on the
string; use 'iri-for-name' to perform ontology specific expansion"
  [name]
  (util/with-types
    [name [String java.net.URL java.io.File]]
    (IRI/create name)))

(extend-type
    String
  p/IRIable
  (p/as-iri [entity] (iri entity)))

(extend-type
    IRI
  p/IRIable
  (p/as-iri [entity] entity))

(extend-type
    OWLNamedObject
  p/IRIable
  (p/as-iri [entity] (.getIRI entity)))

;; #+end_src

;; Extension of ~p/as-iri~ to ~OWLOntology~ is slightly questionable, since an
;; Ontology is identified *not* by an IRI but the combination of an IRI and
;; version IRI.

;; #+begin_src clojure
(extend-type
    OWLOntology
  p/IRIable
  (p/as-iri [entity]
    (-> entity
        .getOntologyID
        .getOntologyIRI
        .get)))

(defn iriable?
  "Returns true iff entity is an IRIable."
  [entity]
  (satisfies? p/IRIable entity))

(defn as-iriable
  "If entity is a iriable do nothing, else throw
an exception."
  [entity]
  (if (iriable? entity)
    entity
    (throw (IllegalArgumentException. "Expecting a IRIable entity"))))

(defrecord Annotated [entity annotations]
  p/Entityable
  (p/as-entity [this] entity)
  p/Annotatable
  (p/as-annotations [this] annotations))

(defn annotate
  "Given an entity or a list of entities, add annotations and return as an
  Entityable and Annotatable entity.

  These entities are used to annotate the axiom describing the relationship
  between the entity and the places where this entity is used. So, for
  example, if an annotated class is added as a subclass of another, the
  SubClassOf axiom will be annotated."
  [entity-or-list & annotation]
  (let [annotation-set (set annotation)]
    (if (seq? entity-or-list)
      (map #(Annotated. % annotation-set)
           (flatten entity-or-list))
      (Annotated. entity-or-list (set annotation)))))

(defn re-annotate
  "Returns a Annotated with the entity and the same annotations as annotated.

  This is generally intended to transfer annotations after, for example, the
  entity has been derived from the existing entity. So, we might want to
  replace an IRI with an OWLObject, but the same annotations."
  [entity ^Annotated annotated]
  (if (= entity (p/as-entity annotated))
    annotated
    (assoc annotated :entity entity)))

(defn- ^java.util.Set union-annotations
  "Returns the union of annotations from annotables.
  Type hinted to java.util.Set so that the result can be called against the
  OWL API."
  [annotatables]
  (apply clojure.set/union
         (map p/as-annotations annotatables)))

(defn- ^java.util.Set hset
  "Same as clojure.core/set with a type hint."
  [coll]
  (set coll))
;; #+end_src


;; * Current Ontology Support

;; In this section, we add support for an implicit ontology argument which is
;; used by the majority of user-facing Tawny functions. The implicit argument is
;; defined on a per namespace basis, which fits with the general paradigm of
;; defining at most one ontology per namespace.

;; There is a lot of argument about this form of implicit argument passing in the
;; Clojure community; obviously it means that the vast majority of functions are
;; not pure, although the use of the OWL API means that this is true anyway. From
;; a practical PoV, requiring users of tawny to pass a ontology to every function
;; call seemed pointless. We would swap this:

;; #+BEGIN_EXAMPLE
;; (defclass A
;;   :super (owl-some r B))
;; #+END_EXAMPLE

;; for something more like this.

;; #+BEGIN_EXAMPLE
;; (defclass A
;;   :super (owl-some r B)
;;   :ontology o)
;; #+END_EXAMPLE

;; More over, co-ercion of strings would knowledge of the ontology also, and
;; would no longer be implicit. So, we would have:

;; #+BEGIN_EXAMPLE
;; (defclass A
;;   :super (owl-some (iri o "r") (iri o "B")
;;   :ontology o)))
;; #+END_EXAMPLE

;; It seems too much, an uncertain gain.

;; The flip-side is that supporting this functionality requires some fairly nasty
;; code. It is, at least, all in one place.

;; We used to support dynamic rebinding of the current ontology, but that
;; really was a step too far, and caused considerable grief with laziness as
;; might be expected.

;; #+begin_src clojure
(defn get-current-ontology-maybe
  "Gets the current ontology, or nil if there is not one."
  ([]
     (get-current-ontology-maybe *ns*))
  ([ns]
     ;; have taken out the current bound ontology as we don't need it now and
     ;; it comes at a cost
     (get @ontology-for-namespace ns)))

(defn get-current-ontology
  "Gets the current ontology. Throws an exception if there is not current
ontology"
  ([]
     (get-current-ontology *ns*))
  ([ns]
     (or (get-current-ontology-maybe ns)
         ;; so break
         (throw (IllegalStateException. "Current ontology has not been set")))))

(defonce
  ^{:doc "Hook called when the default ontology is used"}
  default-ontology-hook (util/make-hook))

;; #+end_src

;; The following section is aggressively optimized, because these functions
;; are called for almost every other method invocation. Where ever possible,
;; we avoid variadic methods; this has a substantial impact on performance as it
;; avoids boxing and unboxing of arguments into and out of lists.

;; There is some code duplication as a result, so this section needs changing
;; with great care.

;; Some parts of this code could do with macroing out, just to ensure that I
;; maintain consistency.

;; #+begin_src clojure
(defn- default-ontology-base-dispatcher
  "Invoke f ensuring that the first argument is an ontology or nil.
This works wqhere we already know that the first value of args is not an
ontology. So, we search for :ontology frame or call ffco to fetch this ontology."
  [ffco f & args]
  (util/run-hook default-ontology-hook)
  (apply f (ffco) args))

(defmacro ^{:private true} dispatch
  "Dispatch with the default ontology if necessary."
  [f & args]
  ;; the majority of this macro is duplicated in variadic
  ;; version of dispatch-ontology
  `(if
       (t/ontology? ~(first args))
     (~f ~@args)
     (do
       (util/run-hook default-ontology-hook)
       (~f (get-current-ontology) ~@args))))

(defn default-ontology
  "Invoke f ensuring the first argument is an ontology or nil.
If the first argument is already an ontology use that, if not use the default
ontology, or throw an IllegalStateException. To set the default ontology use
either the defontology macro, or ontology-to-namespace. This function is
multi-arity as a micro optimization, to avoid a variadic invocation."
  ([f]
     (dispatch f))
  ([f a]
     (dispatch f a))
  ([f a b]
     (dispatch f a b))
  ([f a b c]
     (dispatch f a b c))
  ([f a b c d]
     (dispatch f a b c d))
  ([f a b c d e]
     (dispatch f a b c d e))
  ([f a b c d e fa]
     (dispatch f a b c d e fa))
  ([f a b c d e fa g]
     (dispatch f a b c d e fa g))
  ([f a b c d e fa g h]
     (dispatch f a b c d e fa g h))
  ([f a b c d e fa g h i]
     (dispatch f a b c d e fa g h i))
  ([f a b c d e fa g h i j]
     (dispatch f a b c d e fa g h i j))
  ([f a b c d e fa g h i j & args]
     (if (or
          (t/ontology? a)
          (nil? a))
       (apply f a b c d e fa g h i j args)
       (do
         (util/run-hook default-ontology-hook)
         (apply f (get-current-ontology) a b c d e fa g h i j args)))))

(defn fontology
  "Return a function that always calls f with an ontology as the first
  argument."
  [f]
  (partial default-ontology f))

(defmacro defno
  "Define a new function, that is always called with an ontology as the first
  argument. If the ontology is not supplied the default ontology will be used
  instead."
  [& body]
  (let [f (gensym)]
    `(let [vr# (defn ~@body)]
       (alter-var-root
        vr#
        (fn [f#]
          (fontology f#)))
       (alter-meta!
        vr#
        (fn [m#]
          (let [a# (get m# :arglists)]
            (assoc
             m#
             :arglists
             (conj a#
                   (into []
                         (rest (first a#))))))))
       vr#)))


(defmacro defnb
  "Defines a new function that broadcasts the first and consecutive
  arguments."
  [& body]
  (let [f (gensym)]
    `(let [vr# (defn ~@body)]
       (alter-var-root
        vr#
        (fn [~f]
          (broadcast ~f)))
       (alter-meta!
        vr#
        (fn [m#]
          (let [a# (get m# :arglists)]
            (assoc
             m#
             :arglists
             (concat a#
                     (list
                      [(first (first a#)) '&
                       (second (first a#))]))))))
       vr#)))

(defmacro defnb2
  "Define a new function that broadcasts over first, second and consecutive
  arguments."
  [& body]
  (let [f (gensym)]
    `(let [vr# (defn ~@body)]
       (alter-var-root
        vr#
        (fn [~f]
          (broadcast-2 ~f)))
       (alter-meta!
        vr#
        (fn [m#]
          (let [a# (get m# :arglists)
                fs# (first a#)]
            (assoc
             m#
             :arglists
             (concat a#
                     (list
                      [(first fs#)
                       (second fs#)
                       '&
                       (nth fs# 2)]))))))
       vr#)))

;; #+end_src

;; * Broadcasting


;; I've borrowed the term broadcasting from R and it allows a lot of syntactical
;; concision. So, for instance, this:

;; #+BEGIN_EXAMPLE
;; (owl-some r A)
;; #+END_EXAMPLE

;; returns an existential restriction while

;; #+BEGIN_EXAMPLE
;; (owl-some r A B)
;; #+END_EXAMPLE

;; returns a list of two restrictions. This is a small advantage, but the general
;; idea that functions can return lists make it possible to do, for example,
;; covering axioms

;; #+BEGIN_EXAMPLE
;; (some-only r A B C)
;; #+END_EXAMPLE

;; which returns two existential, and one universal restriction. It makes, for
;; example, ~some~ and ~only~ consistent in their usage with ~or~ and ~and~ which
;; are naturally variadic. As with the default ontology, this syntactic
;; conscision comes with a cost in terms of code complexity.

;; We support broadcasting up to seven arguments without the use of variadic
;; function calls again for performance optimisation.

;; This probably also needs macro'ing out, again, both for consistency, but also
;; to cope with higher arities. Ontologies such as GO easily exceed this.

;; #+begin_src clojure

;; New broadcast
(defn- broadcast-full [special f args]
  (doall
   (let [splt (split-at special args)]
     (map
      (apply partial f (first splt))
      (flatten (second splt))))))

(defmacro ^:private broadcast-call [special f & rest]
  (let [[special-args normal-args] (split-at special rest)]
    `(if (not
          (or
           ~@(map
              (fn [arg#]
                `(sequential? ~arg#))
              normal-args)))
       (list
        ~@(map
           (fn [arg#]
             `(~f ~@special-args ~arg#))
           normal-args))
       (broadcast-full ~special ~f (list ~@special-args ~@normal-args)))))

(defn broadcast [f]
  (fn broadcasting
    ([x a]
     (if (not (sequential? a))
       (f x a)
       (broadcast-full 1 f (list x a))))
    ([x a b]
     (broadcast-call 1 f x a b))
    ([x a b c]
     (broadcast-call 1 f x a b c))
    ([x a b c d]
     (broadcast-call 1 f x a b c d))
    ([x a b c d e]
     (broadcast-call 1 f x a b c d e))
    ([x a b c d e f']
     (broadcast-call 1 f x a b c d e f'))
    ([x a b c d e f' g]
     (broadcast-call 1 f x a b c d e f' g))
    ([x a b c d e f' g h]
     (broadcast-call 1 f x a b c d e f' g h))
    ([x a b c d e f' g h i]
     (broadcast-call 1 f x a b c d e f' g h i))
    ([x a b c d e f' g h i & rest]
     (broadcast-full 1 f (list* x a b c d e f' g h i rest)))))

(defn broadcast-2 [f]
  (fn broadcasting-2
    ([x a b]
     (if (not
          (or (sequential? a)
              (sequential? b)))
       (f x a b)
       (broadcast-full 2 f (list x a b))))
    ([x a b c]
     (broadcast-call 2 x a b c))
    ([x a b c d]
     (broadcast-call 2 x a b c d))
    ([x a b c d e]
     (broadcast-call 2 f x a b c d e))
    ([x a b c d e f']
     (broadcast-call 2 f x a b c d e f'))
    ([x a b c d e f' g]
     (broadcast-call 2 f x a b c d e f' g))
    ([x a b c d e f' g h]
     (broadcast-call 2 f x a b c d e f' g h))
    ([x a b c d e f' g h i]
     (broadcast-call 2 f x a b c d e f' g h i))
    ([x a b c d e f' g h i & rest]
     (broadcast-full 2 f (list* x a b c d e f' g h i rest)))))
;; #+end_src

;; * OWL (No)thing

;; #+begin_src clojure
(defn owl-thing
  "Returns OWL thing."
  []
  (.getOWLThing (owl-data-factory)))

(defn owl-nothing
  "Returns OWL nothing."
  []
  (.getOWLNothing (owl-data-factory)))
;; #+end_src

;; * Axiom mainpulation

;; Just some utility functions for adding or removing axioms.

;; #+begin_src clojure
(defn add-axiom
  "Adds an axiom from the given ontology, or the current one."
  [^OWLOntology o  ^OWLAxiom axiom]
  (.applyChange (owl-ontology-manager)
                (AddAxiom. o axiom))
  axiom)

(defn remove-axiom
  "Removes a list of axioms from the given ontology, or the current one."
  [o & axiom-list]
  (doall
   (map (fn [axiom]
          (.applyChange (owl-ontology-manager)
                        (RemoveAxiom. o axiom)))
        (flatten axiom-list))))

(defn remove-entity
  "Remove from the ontology an entity created and added by
owl-class, defclass, object-property or defoproperty. Entity is the value
returned by these functions.

This removes all the axioms that were added. So, for example, a form such as

   (defclass a
      :subclass b
      :equivalent c)

adds three axioms -- it declares a, makes it a subclass of b, and equivalent
of c."
  [o ^OWLEntity entity]
  (let [remover
        (OWLEntityRemover. (hset (list o)))]
    (.accept entity remover)
    (.applyChanges (owl-ontology-manager)
                   (.getChanges remover))))


;; #+end_src

;; * Ontology options

;; I cannot represent all of the data that I need in an OWLOntology, so I have
;; added this generic "ontology options" support. Essentially, we keep a map for
;; every ontology into which we can dump stuff. This is cleaned when an ontology
;; is removed (an implicit operation happening on the re-eval of a ~defontology~
;; form.

;; I could have implemented this as a wrapper object, as I have done in general
;; for OWL API objects, but chose not to because it makes direct access to the
;; OWL API object harder; the difference between this and most OWL API objects is
;; that I do this is likely to be necessary for lots of OWLOntology objects.

;; #+begin_src clojure
(def ^{:doc "Ontology options. A map on a atom for each ontology"}
  ontology-options-atom (atom {}))

;; return options for ontology -- lazy (defn get-ontology-options [ontology])
(defn ontology-options
  "Returns the ontology options for 'ontology'
or the current-ontology"
  [o]
  (if-let [options
           (get @ontology-options-atom o)]
    options
    (get
     (swap!
      ontology-options-atom assoc o (ref {}))
     o)))

(defn iri-for-name
  "Returns an IRI object for the given name.

This is likely to become a property of the ontology at a later date, but at
the moment it is very simple."
  [o name]
  (if-let [iri-gen (:iri-gen (deref (ontology-options o)))]
    (iri-gen o name)
    (iri (str (p/as-iri o) "#" name))))
;; #+end_src

;; * Interning OWL Entities

;; Support for interning of OWL Entities. This differs from a normal intern by
;; adding some metadata to the var, and calling a hook.

;; #+begin_src clojure
(defonce
  ^{:doc "Hook called when an new var is created with an OWLObject, with the var"}
  intern-owl-entity-hook
  (util/make-hook))

(defn
  run-intern-hook
  "Run intern-owl-entity hooks and return first argument."
  [var]
  (util/run-hook intern-owl-entity-hook var)
  var)

;; #+end_src

;; In the idea world, these would be one function. However, they can't be. The
;; intern version works where we do not know the symbol at compile time. This
;; is generally useful for read'ing and the like. The symbol created cannot be
;; used in the same form because the compiler doesn't know that it has been
;; defined.

;; #+begin_src clojure
(defn intern-owl-string
  "Interns an OWL Entity. Compared to the clojure.core/intern function this
signals a hook, and adds :owl true to the metadata. NAME must be a strings"
  ([name entity]
   (intern-owl-string
    *ns* name entity))
  ([ns name entity]
     (tawny.owl/run-intern-hook
      (intern ns
              (with-meta
                (symbol name)
                {:owl true})
              entity))))
;; #+end_src

;; While this version uses ~def~ semantics -- the point here is that the ~def~
;; form is expanded at compile time, the compiler recognises that the form has
;; been defined, and so allows subsequent referencing of the symbol within the
;; same form.

;; #+begin_src clojure
(defmacro intern-owl
  "Intern an OWL Entity. Compared to the clojure.core/intern function this
signals a hook, and adds :owl true to the metadata. NAME must be a symbol"
  ([name entity]
     `(tawny.owl/run-intern-hook
       (def ~(vary-meta name
                        merge
                        {:owl true})
         ~entity))))
;; #+end_src

;; * OWL2 datatypes

;; Here we are just shifting from a Java idiom (that is an enum with static
;; values) to a Clojure one (that is keywords). Syntactically, this works quite
;; well.

;; #+begin_src clojure
(def
  ^{:doc "A map of keywords to the OWL2Datatypes values"}
  owl2datatypes
  (into {}
        (for [^org.semanticweb.owlapi.vocab.OWL2Datatype
              k (org.semanticweb.owlapi.vocab.OWL2Datatype/values)]
          [(keyword (.name k)) (.getDatatype k (owl-data-factory))])))
;; #+end_src


;; * Self-Annotation

;; Tawny has a reflexive annotation system where an ontology created in Tawny is
;; annotated with knowledge about the fact that it has been annotated in this
;; way.

;; We do this in a separate file (although the same name space). It contains some
;; code duplication because of boot strap.

;; #+begin_src clojure
(load "owl_self")
;; #+end_src

;; * Annotation

;; ** Annotation Addition

;; We now start with support for each of the various entities in OWL starting
;; with Annotation.

;; We start adding annotation to ~OWLNamedObject~ -- this does not include
;; ~OWLOntology~ to which annotation must be added in a totally different way.

;; #+begin_src clojure
(defnb2
  ^{:doc "Adds an annotation to a named object."
    :private true}
  add-annotation
  [o ^OWLNamedObject named-entity annotation]
  (add-axiom
   o
   (.getOWLAnnotationAssertionAxiom
    (owl-data-factory)
    (p/as-iri named-entity)
    ^OWLAnnotation (p/as-entity annotation)
    (p/as-annotations annotation))))

(defn- add-a-name-annotation
  "Add a tawny-name annotation to named-entity, unless the :noname
ontology option has been specified, in which case do nothing."
  [o named-entity name]
  (when
      (and
       (not (get @(ontology-options o)
                 :noname false))
       (instance? String name))
    (add-annotation o named-entity (tawny-name name))))
;; #+end_src

;; Now we add support for annotating ontologies.

;; #+begin_src clojure
(defnb
  ^:private
  add-ontology-annotation
  "Adds an annotation to an ontology."
  [o annotation]
  (.applyChange
   (owl-ontology-manager)
   (AddOntologyAnnotation. o annotation)))
;; #+end_src

;; And, finally, overarching support for adding annotation to both -- we add this
;; support because I did not want to remember myself nor expect anyone else to
;; have to remember to use two different methods for what feels like the same
;; thing. Not having to remember this is almost certainly worth the slight
;; performance hit that this method takes.

;; #+begin_src clojure
(defn- ^OWLAnnotationProperty ensure-annotation-property
  "Ensures that 'property' is an annotation property,
converting it from an IRI if necessary."
  [property]
  (cond
   (t/ann-prop? property)
   property
   (t/iri? property)
   (.getOWLAnnotationProperty
    (owl-data-factory) property)
   :default
   (throw (IllegalArgumentException.
           (format "Expecting an OWL annotation property: %s" property)))))

(defn annotation
  "Creates a new annotation. If literal is a string it is interpreted as a
String in English. Otherwise, it can be any annotation value or object which
can be co-erced to an IRI"
  ([annotation-property literal]
     (cond
      (instance? String literal)
      (annotation annotation-property literal "en")
      (t/ann-val? literal)
      (.getOWLAnnotation
       (owl-data-factory)
       (ensure-annotation-property annotation-property)
       literal)
      (iriable? literal)
      (annotation annotation-property (p/as-iri literal))
      :default
      (throw (IllegalArgumentException.
              "annotation takes a String, OWLAnnotationValue or an object with an IRI."))))
  ([annotation-property ^String literal ^String language]
     (annotation annotation-property
                 (.getOWLLiteral (owl-data-factory) literal language))))

(defnb2 ^:private add-super-annotation
  "Adds a set of super annotation properties to the given sub annotation
  property."
  [o subproperty superproperty]
  (add-axiom
   o
   (.getOWLSubAnnotationPropertyOfAxiom
    (owl-data-factory)
    subproperty
    (ensure-annotation-property superproperty)
    (p/as-annotations superproperty))))

(def deprecated-add-sub-annotation
  "The same as add-super-annotation used to implement the old
add-sub-annotation functionality."
  add-super-annotation)
;; #+end_src

;; ** Specific Annotations

;; Now we want support for adding annotations of a specific type, against all the
;; annotation types of the built-in OWL annotation types. We do this by first
;; adding an ~annotator~ function; this is in the public interface to support the
;; ease of addition of new functions for other users, then move onto to creating
;; all the specific closures that we need to support OWL2.

;; #+begin_src clojure
(defn annotator
  "Creates a new annotator function.
  Annotation-property maybe an OWLAnnotationProperty object or a string in which
  case it will be coerced into a OWLAnnotationProperty using iri or
  iri-for-name. The function returned can take an optional ontology argument
  which will be used in this case."
  [annotation-property]
  (fn annotator-pattern
    ([literal]
     (annotation annotation-property literal))
    ([literal language]
     (annotation annotation-property literal language))))

(def label-property
  (.getRDFSLabel (owl-data-factory)))

(def
  label
  "Return an OWL label annotation."
  (annotator label-property))

(def owl-comment-property
  (.getRDFSComment (owl-data-factory)))

(def
  owl-comment
  "Return an OWL comment annotation"
  (annotator owl-comment-property))

(def is-defined-by-property
  (.getRDFSIsDefinedBy (owl-data-factory)))

(def is-defined-by
  "Return an is defined by annotation."
  (annotator is-defined-by-property))

(def see-also-property
  (.getRDFSSeeAlso (owl-data-factory)))

(def see-also
  "Returns a see-also annotation."
  (annotator see-also-property))

(def backward-compatible-with-property
  (.getOWLBackwardCompatibleWith (owl-data-factory)))

(def backward-compatible-with
  "Returns a backward compatible with annotation."
  (annotator backward-compatible-with-property))

(def incompatible-with-property
  (.getOWLIncompatibleWith (owl-data-factory)))

(def incompatible-with
  "Returns an incompatible with annotation."
  (annotator incompatible-with-property))

(def version-info-property
  (.getOWLVersionInfo (owl-data-factory)))

(def version-info
  "Returns a version info annotation."
  (annotator version-info-property))

(def deprecated-property
  (.getOWLDeprecated (owl-data-factory)))

(def deprecated
  "Returns a deprecated annotation."
  (annotator deprecated-property))

(def ^:private add-label
  "Add labels to the named entities."
  (broadcast-2
   (fn add-label
     [o named-entity label]
     (add-annotation
      o
      named-entity
      (tawny.owl/label label)))))

(def ^:private add-comment
  "Add comments to the named entities."
  (broadcast-2
   (fn add-comment
     [o named-entity comment]
     (add-annotation o named-entity (owl-comment comment)))))
;; #+end_src

;; ** Annotation Property Support

;; We now begin support for the end-user creation of annotation properties.

;; #+begin_src clojure
(def ^{:private true} annotation-property-handlers
  {
   :super #'add-super-annotation
   :subproperty #'deprecated-add-sub-annotation
   :annotation #'add-annotation
   :comment #'add-comment
   :label #'add-label
   })

(defn annotation-property-explicit
  "Add this annotation property to the ontology"
  [o name frames]
  (let [property
        (ensure-annotation-property
         (if (instance? String name)
           (iri-for-name o name)
           name))]
    ;; add the property
    (.addAxiom (owl-ontology-manager)
               o
               (.getOWLDeclarationAxiom
                (owl-data-factory)
                property))
    ;; add a name annotation
    (add-a-name-annotation o property name)
    ;; apply the handlers
    (doseq [[k f] annotation-property-handlers
            :when (get frames k)]
      (f o property (get frames k)))
    ;; return the property
    property))

(defno annotation-property
  {:doc "Creates a new annotation property."}
  [o name & frames]
  (annotation-property-explicit
   o
   name
   (util/check-keys
    (util/hashify frames)
    (keys annotation-property-handlers))))

(defn- get-annotation-property
  "Gets an annotation property with the given name."
  [o property]
  (.getOWLAnnotationProperty
   (owl-data-factory)
   (iri-for-name o property)))
;; #+end_src

;; * Ontology defentity

;; Provides macro support for turning functions into macros declaring new
;; entities. We make the assumption that the functions return some (OWL API)
;; entity and that the support the first argument as an ontology (i.e. the
;; default ontology) semantics.

;; Unfortunately, we cannot directly port the ontology as first argument
;; semantics, because with the macro we need the first argument to be a) required
;; and b) by the symbol. We can't distinguish this at macro-expansion time, since
;; we cannot evaluate the first symbol to find out whether it evals to an
;; ontology.

;; So, instead, we have a "first frame can be an ontology" semantics instead. It
;; has to be the first frame, because we need to know about the first frame
;; *before* we do any thing else, because we need to pass it the function and we
;; do not want to do a full parse to macro time. A bit messy but a reasonable
;; compromise.

;; We should move this before annotation!

;; #+begin_src clojure
(defn- extract-ontology-frame
  "Extracts the ontology frames from a list of frames.
Currently, we define this to be the second value iff the first is an :ontology
keyword. Returns a map of :ontology and the ontology or nil, and :args with
the args minus the ontology frame if it exists."
  [ontology-keyword frames]
  (if (= ontology-keyword (first frames))
    {:ontology (second frames)
     :frames (nthrest frames 2)}
    {:ontology nil
     :frames frames}))

(defn- entity-generator [entity frames entity-function ontology-keyword]
  (let [ontsplit (extract-ontology-frame ontology-keyword frames)
        ont (:ontology ontsplit)
        frames (:frames ontsplit)
        entity-name (name entity)
        ]
    `(let [entity#
           ~(if ont
              `(~entity-function
                ~ont
                ~entity-name
                ~@frames)
              `(~entity-function
               ~entity-name
               ~@frames))]
       (tawny.owl/intern-owl ~entity entity#))))

(defmacro defentity
  "Defines a new macro for defining OWL entities.

This macro allows easy definition of new macros similar to the defclass or
defoproperty macro. It expects an entity-function which can accept an
ontology or nil as a first argument, and then a set of frames. See
`defdontfn' or `defmontfn' which support the declaration of these functions.
The ontology argument is taken from a first :ontology frame which is handled by
this macro. This macro also marks the resultant var with :owl metadata.

The resultant macro takes arguments of the form [name :ontology o :frame1 f].
The ontology frame is optional."
  ([name docstring entity-function]
   `(defentity ~name ~docstring ~entity-function :ontology))
  ([name docstring entity-function ontology-keyword]
   (let [args '([entity & frames])]
     `(let [vr#
            (defmacro
              ~name
              ~docstring
              [entity# & frames#]
              (#'tawny.owl/entity-generator
               entity# frames#
               ~entity-function
               ~ontology-keyword
               ))]
        (alter-meta!
         vr#
         (fn [m#]
           (assoc m#
                  :arglists
                  (quote ~args))))
        vr#))))


;; #+end_src

;; * Last bit of annotation

;; And, finally, we complete the annotation frame.

;; #+begin_src clojure
(defentity defaproperty
  "Defines a new annotation property in the current ontology.
See 'defclass' for more details on the syntax"
  'tawny.owl/annotation-property)
;; #+end_src

;; * Ontology Manipulation

;; Tools for generating and manipulating ontology objects.

;; ** Ontology to Namespace mapping

;; Default ontology semantics requires us to have a place to store the default
;; ontology and a semantics for determining the scope of the defaultness -- a
;; single global default was never going to work.

;; We use the namespace scope for the simple reason that all the Clojure tools in
;; use already cope with this existance of this scope, as clojure requires it --
;; if you eval a single function, you need to know which function the namespace
;; should be declare and evaluated in. That this (generally) maps to a single
;; source file gives it an intuitive semantics also.

;; #+begin_src clojure
(defn ontology-to-namespace
  "Sets the current ontology as defined by `defontology'"
  ([o]
     (ontology-to-namespace *ns* o))
  ([ns o]
     (dosync
      (alter
       ontology-for-namespace
       assoc ns o))))

(defn- remove-ontology-from-namespace-map
  "Remove an ontology from the namespace map"
  [o]
  (dosync
   (doseq
       [ns
        ;; select namespaces with given ontology
        (for [[k v] @ontology-for-namespace
              :when (= v o)]
          k)]
     (alter ontology-for-namespace
            dissoc ns))))

;; #+end_src

;; We need to use a hook to signal that ontologies are removed, rather than just
;; call a clean up method because we may need to clean up state from other
;; namespaces. The driving use-case for this is the reasoner namespace, which
;; needs to dump the reasoner at the same time or it will memory leak. But, we
;; cannot call ~tawny.reasoner~ here or we will have a circular dependency.

;; #+begin_src clojure
(def
  ^{:doc "Hook called immediately after an ontology is removed from the
owl-ontology-manager."}
  remove-ontology-hook (util/make-hook))

(defn remove-ontology-maybe
  "Removes the ontology with the given ID from the manager.
This calls the relevant hooks, so is better than direct use of the OWL API. "
  [^OWLOntologyID ontologyid]
  (when (.contains (owl-ontology-manager) ontologyid)
    (let [o (.getOntology (owl-ontology-manager) ontologyid)]
      (.removeOntology
       (owl-ontology-manager) o)
      ;; remove the ontology options
      (dosync
       (swap! ontology-options-atom
              dissoc o))
      ;; remove the ontology from the namespace map
      (remove-ontology-from-namespace-map o)
      (util/run-hook remove-ontology-hook o)
      o)))

(defn- add-an-ontology-name
  "Adds an tawny-name annotation to ontology, unless the :noname ontology
  options is specified in which case do nothing."
  [o n]
  (when
      (and
       (not (get @(ontology-options o)
                 :noname false))
       n)
    (add-ontology-annotation
     o (tawny-name n))))

(defn- set-iri-gen
  "Add an IRI gen function to the ontology options."
  [o f]
  (if f
    (dosync
     (alter (ontology-options o)
            merge {:iri-gen f}))))

(defn- set-prefix
  "Sets a prefix for the ontology."
  [^OWLOntology o ^String p]
  (if p
    (.setDefaultPrefix
     (.asPrefixOWLOntologyFormat
      (.getOntologyFormat
       (owl-ontology-manager) o))
     p)))

;; #+end_src

;; It's an open question as to whether we really need all of these frames. They
;; where added in the first place because the default ontology support made them
;; complex otherwise -- it was impossible for the user to call (for example)
;; ~owl-comment~ because the default ontology had not been set yet. Since then,
;; we have added "maybe" default ontology functionality also, so the frames are
;; partly historic.

;; #+begin_src clojure
(defn- add-ontology-comment
  "Adds a comment annotation to the ontology"
  [o s]
  (if s
    (add-ontology-annotation o (owl-comment s))))

(defn- add-see-also
  "Adds a see also annotation to the ontology"
  [o s]
  (if s
    (add-ontology-annotation o (see-also s))))

(defn- add-version-info
  "Adds a version info annotation to the ontology."
  [o v]
  (if v
    (add-ontology-annotation o (version-info v))))

;; owl imports
(defn owl-import
  "Adds a new import to the current ontology. o may be an
  ontology or an IRI"
  ([o]
   (owl-import (get-current-ontology) o))
  ([ontology-into o]
   (.applyChange (owl-ontology-manager)
                 (AddImport. ontology-into
                             (.getOWLImportsDeclaration
                              (owl-data-factory)
                                (p/as-iri o))))))
(defn- add-import [o olist]
  (doseq [n olist]
    (owl-import o n)))
;; #+end_src

;; ** Ontology Frame

;; And, finally, the implementation of the Ontology frame itself.

;; #+begin_src clojure
(def ^{:private true} ontology-handlers
  {
   ;; these are not broadcast
   :iri-gen #'set-iri-gen,
   :prefix #'set-prefix,
   ::name #'add-an-ontology-name
   :seealso #'add-see-also
   :comment #'add-ontology-comment
   :versioninfo #'add-version-info
   ;; these two are specially dealt with and are broadcast
   :annotation #'add-ontology-annotation
   :import #'add-import
   })

(defn- flatten-first-list-values
  "Flattens the values for keys, except those in noflatten"
  [mp noflattenkeys]
  (into {}
        (for [[k v] mp]
          (if-not (noflattenkeys k)
            (if (= 1 (count v))
              [k (first v)]
              (throw (IllegalArgumentException.
                      (str "Frame " k " accepts only a single argument"))))
            [k v]))))

(defn ontology-explicit
  "Returns a new ontology. See 'defontology' for full description."
  [options]
  (let [
        ;; having got all values as lists, pull them all apart again
        options (flatten-first-list-values
                 options #{:annotation :import})
        ;; the prefix is specified by the prefix or the name.
        ;; this allows me to do "(defontology tmp)"
        options (merge options
                       {:prefix (or (:prefix options)
                                    (::name options))})
        iri (iri (get options :iri
                             (str
                              (java.util.UUID/randomUUID)
                              (if-let [name
                                       (get options ::name)]
                                (str "#" name)))))
        viri-str (get options :viri)
        viri (when viri-str (tawny.owl/iri viri-str))
        noname (get options :noname false)]
    (remove-ontology-maybe
     (if viri
       (OWLOntologyID. iri viri)
       (OWLOntologyID. iri)))
    (let [ontology
          (if viri
            (.createOntology (owl-ontology-manager)
                             (OWLOntologyID. iri viri))
            (.createOntology (owl-ontology-manager) iri))]
      (if noname
        (dosync
         (alter
          (tawny.owl/ontology-options ontology)
          merge {:noname true}))
        (owl-import ontology
                    (tawny-ontology)))
      (doseq [[k f] ontology-handlers
              :let [opt
                    (get options k)]
              :when opt]
        (f ontology opt))
      ontology)))

(defn ontology [& args]
  (ontology-explicit
   (util/check-keys
    (util/hashify args)
    (list* :iri :noname :viri
           (keys ontology-handlers)))))

;; #+end_src

;; Some code duplication here with ~defentity~ but we need to avoid default
;; ontology support which does not make sense at all.

;; #+begin_src clojure
(defn ontology-def-f [name body]
  `(let [ontology# (ontology ::name ~(clojure.core/name name) ~@body)]
     (ontology-to-namespace ontology#)
     (intern-owl ~name ontology#)))

(defmacro defontology
  "Define a new ontology with `name'.

The following keys must be supplied.
:iri -- the IRI for the new ontology
:prefix -- the prefix used in the serialised version of the ontology
"
  [name & body]
  (ontology-def-f name body))

;;; Begin ontology look up functions
(defn- check-entity-set
  [entity-set iri]
  ;; ontology could be in full, or could be punning. Either way we are
  ;; stuffed.
  (when (< 1 (count entity-set))
    (throw
     (IllegalArgumentException.
      (format "Can not uniquely determine type of IRI: %s,%s" iri entity-set))))
  ;; IRI appears once; happiness or
  ;; IRI appears not at all
  (when (= 1 (count entity-set))
    (first entity-set)))

(defn entity-for-iri
  "Return the OWLObject for a given IRI if it exists, checking
'ontology' first, but checking all loaded ontologies.

This function uses a heuristic to find the right entity. If you want
more control use 'check-entity-set' and the '.getEntitiesInSignature'
method of OWLOntology."
  [^OWLOntology o ^IRI iri]
  (or
   ;; single item in current ontology
   (check-entity-set
    (.getEntitiesInSignature o iri)
    iri)
   ;; single item in current or imports
   (check-entity-set
    (.getEntitiesInSignature o iri true)
    iri)
   ;; single item in anything we know about
   ;; perhaps this is not sure a good idea, since the result will depend on
   ;; loaded ontologies, which might change for different invocations.
   (check-entity-set
    (apply
     clojure.set/union
     (map #(.getEntitiesInSignature ^OWLOntology % iri true)
          (vals @ontology-for-namespace)))
    iri)))

(defn entity-for-string
  "Returns the OWLObject for a given string.
See 'entity-for-iri' for more details. Attempts both ontology specific iri to name
conversion, and direct use of string as an IRI."
  [o string]
  (or (entity-for-iri o (iri-for-name o string))
      ;; string name somewhere?
      (entity-for-iri o (iri string))))

(defn ^String get-prefix
  "Returns the prefix for the given ontology, or the current ontology if none
is given."
  [^OWLOntology o]
  ;; my assumption here is that there will only ever be one prefix for a given
  ;; ontology. If not, it's all going to go wrong.
  (.getDefaultPrefix
   (.asPrefixOWLOntologyFormat
    (.getOntologyFormat (owl-ontology-manager)
                        o))))

(defno save-ontology
  "Save the current 'ontology' in the file or `filename' if given.
If no ontology is given, use the current-ontology"
  ([o filename]
     (save-ontology o filename (ManchesterSyntaxDocumentFormat.)
                    (str "## This file was created by Tawny-OWL\n"
                         "## It should not be edited by hand\n" )))
  ([o filename format]
     (save-ontology o filename format ""))
  ([^OWLOntology o ^String filename format prepend]
     (let [file (File. filename)
           output-stream (new FileOutputStream file)
           file-writer (new PrintWriter output-stream)
           ^OWLDocumentFormat
           this-format
           (cond
            (= format :rdf) (RDFXMLDocumentFormat.)
            (= format :omn) (ManchesterSyntaxDocumentFormat.)
            (= format :owl) (OWLXMLDocumentFormat.)
            (= format :ttl) (TurtleDocumentFormat.)
            :else format)]
       (when (.isPrefixOWLOntologyFormat this-format)
         (doseq [ont (vals @ontology-for-namespace)
                 :when (get-prefix ont)]
           (.setPrefix
            (.asPrefixOWLOntologyFormat this-format) (get-prefix ont)
            (str (p/as-iri ont) "#")))
         (.setPrefix (.asPrefixOWLOntologyFormat this-format) (get-prefix o)
                     (str (p/as-iri o) "#")))
       (.print file-writer prepend)
       (.flush file-writer)
       (.saveOntology (owl-ontology-manager) o
                      this-format output-stream))))
;; #+end_src

;; * OWL Entity guess/ensure

;; ** Guess

;; The guess functionality is quite nasty, but it allows us to make an import
;; syntactial simplification for the user -- we can use ~owl-some~, for instance,
;; as the same syntax for both datatype and object properties.

;; This would be reasonably straight-forward if we forced users to pass in OWL
;; API Objects, but we don't. They can also pass in strings (tawny names), IRI
;; objects or strings which should be treated as IRIs. So, we have to fix all of
;; this and use this to guess our types.

;; #+begin_src clojure
(derive ::class ::object)
(derive ::object-property ::object)
(derive ::object-property ::property)
(derive ::data-property ::property)
(derive ::data-property ::data)

(defn guess-type
  "Guesses the type of the entity. Returns ::object, :data or :annotation or
nil where the type cannot be guessed. IllegalArgumentException is thrown for
arguments which make no sense (not an OWLObject, IRI, String or number).

What this means is, for a collection find the first entity for which we can
guess a type for. For an OWLClass, OWLIndividual, OWLDatatype or
OWLAnnotationProperty object return the appropriate value. For an IRI check
the current ontology, the current ontology with its import closure, and all
known ontologies with their import clojure. For a string convert to an IRI
using the current ontology rules, and check again. Finally, check convert to
an IRI with no transformation. nil is returned when the result is not clear.
"
  [entity]
  (let [entity (p/as-entity entity)]
    (cond
     ;; it's a collection -- find the first entity
     (coll? entity)
     (some guess-type entity)
     ;; return if individual, class, datatype
     (t/class-exp? entity)
     ::class
     (t/obj-prop-exp? entity)
     ::object-property
     (t/ann-prop? entity)
     ::annotation
     (t/data-prop-exp? entity)
     ::data-property
     (t/data-range? entity)
     ::data
     ;; keyword -- these are builtin OWL2Datatypes
     (and (keyword? entity)
          (get owl2datatypes entity))
     ::data
     ;; owl individuals tell us nothing, cause we still don't know!
     (t/individual? entity)
     nil
     (number? entity)
     nil
     ;; if we get nil, carry on, because we may be able to determine the type
     ;; from a later argument.
     (nil? entity)
     nil
     ;; This is to catch old calls which used to pass the default ontology
     (instance? OWLOntology entity)
     (throw (IllegalArgumentException.
             (str "Ontology is no longer a valid argument at this position.")))
     ;; probably it's something crazy here.
     :default
     (throw (IllegalArgumentException.
             (str "Cannot guess this type:" entity))))))

(defn guess-individual-literal
  [entity]
  (let [entity (p/as-entity entity)]
    (cond
     (coll? entity)
     (some guess-individual-literal entity)
     (t/individual? entity)
     ::individual
     (t/literal? entity)
     ::literal
     (number? entity)
     ::literal
     (or (= true entity)
         (= false entity))
     ::literal
     (string? entity)
     ::literal
     :default
     (throw (IllegalArgumentException.
             (str "Cannot tell if this is individual or literal:" entity))))))
;; #+end_src


;; ** Ensure

;; Transform *anything* into *something* or crash!

;; #+begin_src clojure


(defn-
  ^{:private true}
  ^OWLObjectProperty ensure-object-property
  "Ensures that the entity in question is an OWLObjectProperty
or throw an exception if it cannot be converted."
  [prop]
  (let [prop (p/as-entity prop)]
    (cond
     (fn? prop)
     (ensure-object-property (prop))
     (t/obj-prop-exp? prop)
     prop
     (t/iri? prop)
     (.getOWLObjectProperty (owl-data-factory) prop)
     :default
     (throw (IllegalArgumentException.
             (str "Expecting an object property. Got: " prop))))))

(defn- ensure-class-except [clz]
  (throw (IllegalArgumentException.
          (str "Expecting a class. Got: " clz))))

(defn- ^OWLClass ensure-class
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  [clz]
  ;; convert to entity if necessary
  (let [clz (p/as-entity clz)]
    (cond
     (t/class-exp? clz)
     clz
     (t/iri? clz)
     (.getOWLClass (owl-data-factory) clz)
     (fn? clz)
     (try
       (ensure-class (clz))
       (catch clojure.lang.ArityException e
         (ensure-class-except clz)))
     true (ensure-class-except clz))))

(defn-
  ^OWLDataProperty ensure-data-property
  "Ensures that 'property' is an data property,
converting it from a string or IRI if necessary."
  [property]
  (let [property (p/as-entity property)]
    (cond
     (t/data-prop-exp? property)
     property
     (t/iri? property)
     (.getOWLDataProperty
      (owl-data-factory) property)
    :default
     (throw (IllegalArgumentException.
             (format "Expecting an OWL data property: %s" property))))))

(defn-
  ^OWLPropertyExpression
  ensure-property
  "Ensures that the entity in question is an OWLPropertyExpression.
If prop is ambiguous (for example, a string or IRI that whose type has not
been previously defined) this will create an OWLObjectProperty rather than
an OWLDataProperty"
  [prop]
  (let [prop (p/as-entity prop)
        type
        (or
          ;; guess the type -- if we can't then object-property it's because
          ;; we don't know and not because it's illegal
          (guess-type prop)
          ::object-property)]
    (case type
      ::object-property
      (ensure-object-property prop)
      ::data-property
      (ensure-data-property prop))))

(defn- ^OWLDatatype ensure-datatype
  "Ensure that 'datatype' is an OWLDatatype. Will convert from an keyword for
  builtin datatypes."
  [datatype]
  (let [datatype (p/as-entity datatype)]
    (cond
     (t/data-type? datatype)
     datatype
     (instance? org.semanticweb.owlapi.vocab.OWL2Datatype datatype)
     datatype
     (keyword? datatype)
     (if-let [d (get owl2datatypes datatype)]
       (ensure-datatype d)
       (throw (IllegalArgumentException.
               (str "Was expecting a datatype. Got " datatype "."))))
     (instance? IRI datatype)
     (.getOWLDatatype (owl-data-factory) ^IRI datatype)
     :default
     (throw (IllegalArgumentException.
             (str "Was expecting a datatype. Got " datatype "."))))))

(defn- ^org.semanticweb.owlapi.model.OWLDataRange ensure-data-range
  "Ensure that 'data-range' is a OWLDataRange either directly or
as a datatype."
  [data-range]
  (cond
   (t/data-range? data-range)
   data-range
   :default
   (ensure-datatype data-range)))

(defn- ^OWLIndividual ensure-individual
  "Returns an INDIVIDUAL.
If INDIVIDUAL is an OWLIndividual return individual, else
interpret this as a string and create a new OWLIndividual."
  [individual]
  (let [individual (p/as-entity individual)]
    (cond
      (t/individual? individual)
      individual
      (t/iri? individual)
      (.getOWLNamedIndividual (owl-data-factory)
                              individual)
      :default
      (throw (IllegalArgumentException.
              (str "Expecting an Individual. Got: " individual))))))
;; #+end_src

;; * OWL Class

;; We start ~OWLClass~ support rather randomly at this point.

;; #+begin_src clojure
(defnb2
  ^{:doc "Adds one or more superclasses to name in ontology."
   :arglists '([ontology name & superclass])}
  add-superclass
  [o name superclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              (owl-data-factory)
              (ensure-class name)
              (ensure-class superclass)
              (p/as-annotations superclass))))

(defnb2
  ^{:doc "Adds one or more subclasses to name in ontology."
    :arglists '([name & subclass] [ontology name & subclass])}
  add-subclass
  [o name subclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              (owl-data-factory)
              (ensure-class subclass)
              (ensure-class name)
              (p/as-annotations subclass))))
;; #+end_src

;; We had to deprecated the old ~add-subclass~ because it has exactly backward
;; semantics from now. This is because the Manchester syntax is backward, so I
;; got it wrong.

;; #+begin_src clojure
(def deprecated-add-subclass
  ^{:doc "This maintains the functionality of the old add-subclass function
which actually added superclasses. The new add-subclass does the
opposite of this."
    :deprecated "1.1"
    :arglists '([name & superclass] [ontology name & superclass])}
  #'add-superclass)

(defnb2
  ^{:doc "Adds an equivalent axiom to the ontology."
    :arglists '([ontology name equivalent])}
  add-equivalent
  [o name equivalent]
  (add-axiom o
             (.getOWLEquivalentClassesAxiom
              (owl-data-factory)
              (ensure-class name)
              (ensure-class equivalent)
              (p/as-annotations equivalent))))

(defnb2
  ^{:doc "Adds a disjoint axiom to the ontology."
    :arglists '([ontology name disjoint])}
  add-disjoint
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointClassesAxiom
    (owl-data-factory)
    (hash-set (ensure-class name)
              (ensure-class disjoint))
    (p/as-annotations disjoint))))

(defn add-disjoint-union
  "Adds a disjoint union axiom to all subclasses."
  [o clazz subclasses]
  (let [ensured-subclasses
        (util/domap #(ensure-class %) subclasses)
        ]
    (list
     (add-axiom o
                (.getOWLDisjointUnionAxiom
                 (owl-data-factory)
                 (ensure-class clazz)
                 (set
                  (map
                   ensure-class
                   subclasses))
                 (union-annotations subclasses))))))

;; a class can have only a single haskey, so ain't no point broadcasting this.
(defn add-has-key
  "Adds a has-key to the class."
  [o class propertylist]
  ;; nil or empty list safe
  (if (seq propertylist)
    (let [type (guess-type propertylist)
          propertylist
          (cond
           (isa? type ::object)
           (map ensure-object-property propertylist)
           (isa? type ::data)
           (map (partial ensure-data-property o) propertylist)
           :default
           (throw
            (IllegalArgumentException.
             "Unable to determine type of property")))]
      (add-axiom o
                 (.getOWLHasKeyAxiom
                  (owl-data-factory)
                  (ensure-class class)
                  (set propertylist)
                  (p/as-annotations propertylist))))))
;; #+end_src

;; We end rather early here, and should probably bring the rest of the OWLClass
;; code in.

;; * Object Properties

;; And now we move onto ~OWLObjectProperty~ support.

;; #+begin_src clojure
(defnb2 add-domain
  "Adds all the entities in domainlist as domains to a property."
  [o property domain]
  (add-axiom o
             (.getOWLObjectPropertyDomainAxiom
              (owl-data-factory)
              (ensure-object-property property)
              (ensure-class domain)
              (p/as-annotations domain))))

(defnb2 add-range
  "Adds all the entities in rangelist as range to a property."
  [o property range]
  (add-axiom o
             (.getOWLObjectPropertyRangeAxiom
              (owl-data-factory)
              (ensure-object-property property)
              (ensure-class range)
              (p/as-annotations range))))

(defnb2
  add-inverse
  "Adds all the entities in inverselist as inverses to a property."
  [o property inverse]
  (add-axiom o
             (.getOWLInverseObjectPropertiesAxiom
              (owl-data-factory)
              (ensure-object-property property)
              (ensure-object-property inverse)
              (p/as-annotations inverse))))

(defn inverse
  "Creates an object inverse of expression."
  [property]
  (.getOWLObjectInverseOf
   (owl-data-factory)
   (ensure-object-property property)))

(defnb2 add-superproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property superproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              (owl-data-factory)
              (ensure-object-property property)
              (ensure-object-property superproperty)
              (p/as-annotations superproperty))))

(defnb2 add-subproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property subproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              (owl-data-factory)
              (ensure-object-property subproperty)
              (ensure-object-property property)
              (p/as-annotations subproperty))))

(def ^{:deprecated "1.1"
       :doc "This is the same as add-superproperty but marked as deprecated
and used as the handler for :subproperty."
       } deprecated-add-superproperty
  add-superproperty)

;; broadcasts specially
(defn add-subchain
  "Adds a property chain to property."
  [o property subpropertylist]
  (when subpropertylist
    (let [property (ensure-object-property property)
          lists (filter sequential? subpropertylist)
          properties (filter (comp not sequential?) subpropertylist)
          ]
      (list
       ;; add individual entities are a single chain
       (when (seq properties)
         (add-axiom
          o
          (.getOWLSubPropertyChainOfAxiom
           (owl-data-factory)
           (map ensure-object-property properties)
           property
           (p/as-annotations properties))))
       ;; add sequential entities as a chain in their own right
       (doall
        (map (partial add-subchain
                      o property)
             lists))))))

(def
  ^{:doc "This is the same as add-subchain, but marked as deprecated
and used as the handler for :subpropertychain."
    :deprecated "1.1"} deprecated-add-subpropertychain
  add-subchain)

(def add-equivalent-property
  "Adds a equivalent data properties axiom."
  (fontology
   (broadcast-2
    (fn add-equivalent-property
      [o property equivalent]
      (add-axiom
       o (.getOWLEquivalentObjectPropertiesAxiom
          (owl-data-factory)
          (ensure-object-property property)
          (ensure-object-property equivalent)
          (p/as-annotations equivalent)))))))

(defn equivalent-properties
  "Adds properties as equivalent to the ontology."
  [o properties]
  (let [properties
        (map ensure-object-property properties)]
    (add-axiom
     o (.getOWLEquivalentObjectPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (union-annotations properties)))))

(def add-disjoint-property
  "Adds a disjoint property axiom to the ontology"
  (fontology
   (broadcast-2
    (fn add-disjoint-property
      [o name disjoint]
      (add-axiom
       o
       (.getOWLDisjointObjectPropertiesAxiom
        (owl-data-factory)
        (hash-set
         (ensure-object-property name)
         (ensure-object-property disjoint))
        (p/as-annotations disjoint)))))))

(defn disjoint-properties
  "Make all the properties in PROPERTIES disjoint."
  [o properties]
  (let [properties
        (doall
         (map ensure-object-property properties))]
    (add-axiom
     o (.getOWLDisjointObjectPropertiesAxiom
        (owl-data-factory)
        (set properties)
        (union-annotations properties)))))

(def
  ^{:private true}
  charfuncs
  {:transitive
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLTransitiveObjectPropertyAxiom
      df
      op
      an))
   :functional
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLFunctionalObjectPropertyAxiom
      df
      op
      an))
   :inversefunctional
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLInverseFunctionalObjectPropertyAxiom
      df
      op
      an))
   :symmetric
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLSymmetricObjectPropertyAxiom
      df
      op
      an))
   :asymmetric
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLAsymmetricObjectPropertyAxiom
      df
      op
      an))
   :irreflexive
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLIrreflexiveObjectPropertyAxiom
      df
      op
      an))
   :reflexive
   (fn [^OWLDataFactory df
        ^OWLObjectProperty op
        ^java.util.Set an]
     (.getOWLReflexiveObjectPropertyAxiom
      df
      op
      an))})

(defnb2 add-characteristics
  "Add a list of characteristics to the property."
  [o property characteristic]
  (if-let [fchar (get charfuncs
                      (p/as-entity characteristic))]
    ;;; this is all totally wrong headed here because we are passing in the
;;; property which does not carry the annotations. So need to change all the
;;; functions above to take the annotation as an extra argument
    (add-axiom
     o
     (fchar
      (owl-data-factory)
      (ensure-object-property property)
      (p/as-annotations characteristic)))
    (throw (IllegalArgumentException.
            (str "Characteristic is not recognised:" characteristic)))))

(def ^{:private true} object-property-handlers
  {
   :domain #'add-domain
   :range #'add-range
   :inverse #'add-inverse
   :sub #'add-subproperty
   :super #'add-superproperty
   :subproperty #'deprecated-add-superproperty
   :characteristic #'add-characteristics
   :subchain #'add-subchain
   :subpropertychain #'deprecated-add-subpropertychain
   :disjoint #'add-disjoint-property
   :equivalent #'add-equivalent-property
   :annotation #'add-annotation
   :label #'add-label
   :comment #'add-comment
   })

;; object properties
(defn object-property-explicit
   "Returns an object-property. This requires an hash with a list
value for each frame."
  [o name frames]
  (let [o (or (first (get frames :ontology))
              o)
        property
        (ensure-object-property
         (if (instance? String name)
           (iri-for-name o name)
           name))]
    (do
      ;; add the property
      (add-axiom o
                 (.getOWLDeclarationAxiom
                  (owl-data-factory) property))
      ;; add a name annotation
      (add-a-name-annotation o property name)
      ;; apply the handlers
      (doseq [[k f] object-property-handlers
              :when (get frames k)]
        (f o property (get frames k))))
    property))

(defno object-property
  "Returns a new object property in the current ontology."
  [o name & frames]
  (let [keys (list* :ontology (keys object-property-handlers))]
    (object-property-explicit
     o name
     (util/check-keys
      (util/hashify-at keys frames)
      keys))))

(defentity defoproperty
  "Defines a new object property in the current ontology."
  'tawny.owl/object-property)
;; #+end_src

;; * Random guess-type stuff

;; Why is this not earlier?

;; #+begin_src clojure

(defn
  guess-type-args
  {:doc  "Broadcasting version of guess-type"
   :private true}
  ;; unwind to avoid variadic args for the most common calls.
  ([a]
   (guess-type a))
  ([a b]
   (or
    (guess-type a)
    (guess-type b)))
  ([a b c]
   (or
    (guess-type a)
    (guess-type b)
    (guess-type c)))
  ([a b c d]
   (or
    (guess-type a)
    (guess-type b)
    (guess-type c)
    (guess-type d)))
  ([a b c d & args]
   (or
    (guess-type a)
    (guess-type b)
    (guess-type c)
    (guess-type d)
    ;; guess-type already copes with collections
    (guess-type args))))

(defn guess-individual-literal-args
  {:doc "Broadcasting version of guess-individual-literal"
   :private true}
  ([a]
   (guess-individual-literal a))
  ([a b]
   (or
    (guess-individual-literal a)
    (guess-individual-literal b)))
  ([a b c]
   (or
    (guess-individual-literal a)
    (guess-individual-literal b)
    (guess-individual-literal c)))
  ([a b c d]
   (or
    (guess-individual-literal a)
    (guess-individual-literal b)
    (guess-individual-literal c)
    (guess-individual-literal d)))
  ([a b c d & args]
   (or
    (guess-individual-literal a)
    (guess-individual-literal b)
    (guess-individual-literal c)
    (guess-individual-literal d)
    (guess-individual-literal args))))
;; #+end_src

;; * Restriction Overloading

;; We provide extensive guess functionality previously. We define the
;; multi-methods that we use to apply this here.

;; We do not really need multi-methods, as these functions are closed (that is
;; ~owl-some~ should call either ~object-some~ or ~data-some~). At some point,
;; the multi-method should be, perhaps, be removed and replaced with an if
;; statement, although I need to check whether the site-specific caching makes
;; this any faster.

;; #+begin_src clojure
;; multi methods for overloaded entities. We guess the type of the arguments,
;; which can be (unambiguous) OWLObjects, potentially ambiguous IRIs or
;; strings. If we really can tell, we guess at objects because I like objects
;; better.
(defmulti owl-some
  "Returns an existential restriction with another class or a data range."
  #'guess-type-args)

(defmulti only
  "Returns a universal rescriction with another class or data range."
  #'guess-type-args)

(defmulti some-only
  "Returns a list containing existential restrictions to each of the arguments,
and universal relationship to the union of each of the arguments."
  #'guess-type-args)

(defmulti owl-and
  "Returns an intersection restriction to all of the arguments."
  #'guess-type-args)

(defmulti owl-or
  "Returns an union restriction to all of the arguments."
  #'guess-type-args)

(defmulti exactly
  "Returns an exact cardinality restriction."
  #'guess-type-args)

(defmulti oneof
  "Returns a one-of restriction to the arguments of individuals or
data ranges."
  #'guess-individual-literal-args)

(defmulti at-least
  "Returns a minimum cardinality restriction."
  #'guess-type-args)

(defmulti at-most
  "Returns a maximum cardinality restriction."
  #'guess-type-args)

(defmulti has-value
  "Returns a has-value restriction."
  #'guess-type-args)

;; this is the outlier because it is also used for individuals, so is called
;; overloaded on arity
(defmulti
  ^{:private true
    :doc "Returns a data or object complement of restriction."}
  owl-not-one #'guess-type-args)

;; use declare here because I just don't want to owl-not later
(declare fact-not)

(defn owl-not
  "Returns a complement of restriction or negative property assertion axiom."
  ([entity]
     (owl-not-one entity))
  ([property entity]
     (fact-not property entity)))

(defn guess-type-error
  "Throws an exception always"
  [& args]
  (throw (IllegalArgumentException.
          (str "Unable to determine the type of: " args))))
;; #+end_src

;; ** Nil error methods

;; #+begin_src clojure
(defmethod owl-some nil [& rest]
  (apply guess-type-error rest))

(defmethod only nil [& rest]
  (apply guess-type-error rest))

(defmethod some-only nil [& rest]
  (apply guess-type-error rest))

(defmethod owl-and nil [& rest]
  (apply guess-type-error rest))

(defmethod owl-or nil [& rest]
  (apply guess-type-error rest))

(defmethod owl-not-one nil [& rest]
  (apply guess-type-error rest))

(defmethod exactly nil [& rest]
  (apply guess-type-error rest))

(defmethod at-least nil [& rest]
  (apply guess-type-error rest))

(defmethod at-most nil [& rest]
  (apply guess-type-error rest))

(defmethod has-value nil [& rest]
  (apply guess-type-error rest))
;; #+end_src

;; * Object Restrictions

;; All methods producing OWL object restrictions.

;; #+begin_src clojure

;; short cuts for the terminally lazy. Still prefix!
(def && #'owl-and)
(def || #'owl-or)
(def ! #'owl-not)

;; "long cuts" for consistency with some
(def owl-only #'only)

(defnb object-some
  "Returns an OWL some values from restriction."
  [property class]
  (.getOWLObjectSomeValuesFrom
   (owl-data-factory)
   (ensure-object-property property)
   (ensure-class class)))

(util/defmethodf owl-some ::object object-some)

(defnb object-only
  "Returns an OWL all values from restriction."
  [property class]
  (.getOWLObjectAllValuesFrom
   (owl-data-factory)
   (ensure-object-property property)
   (ensure-class class)))

(util/defmethodf only ::object object-only)

;; union, intersection
(defn object-and
  "Returns an OWL intersection of restriction."
  [& classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-and must have at least two classes")))

    (.getOWLObjectIntersectionOf
     (owl-data-factory)
     (java.util.HashSet.
      (util/domap
       #(ensure-class %)
       ;; flatten list for things like owl-some which return lists
       classes)))))

(util/defmethodf owl-and ::object object-and)

(defn object-or
  "Returns an OWL union of restriction."
  [& classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-or must have at least two classes")))

    (.getOWLObjectUnionOf
     (owl-data-factory)
     (java.util.HashSet.
      (util/domap #(ensure-class %)
                  (flatten classes))))))

(util/defmethodf owl-or ::object object-or)

;; lots of restrictions return a list which can be of size one. so all these
;; functions take a list but ensure that it is of size one.
(defn object-not
  "Returns an OWL complement of restriction."
  [& class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectComplementOf
   (owl-data-factory)
   (ensure-class (first (flatten class)))))

(util/defmethodf owl-not-one ::object object-not)

(defn object-some-only
  "Returns an restriction combines the OWL some values from and
all values from restrictions."
  [property & classes]
  (list
   (object-some property classes)
   (object-only property
                (object-or classes))))

(util/defmethodf some-only ::object object-some-only)

(defn object-at-least
  "Returns an OWL at-least cardinality restriction."
  ([cardinality property]
   (object-at-least cardinality property (owl-thing)))
  ([cardinality property class]
   (.getOWLObjectMinCardinality
    (owl-data-factory) cardinality
    (ensure-object-property property)
    (ensure-class class))))

(util/defmethodf at-least ::object object-at-least)

(defn object-at-most
  "Returns an OWL at-most cardinality restriction."
  ([cardinality property]
   (object-at-most cardinality property (owl-thing)))
  ([cardinality property class]
   (.getOWLObjectMaxCardinality
    (owl-data-factory) cardinality
    (ensure-object-property property)
    (ensure-class class))))

(util/defmethodf at-most ::object object-at-most)

(defn object-exactly
  "Returns an OWL exact cardinality restriction."
  ([cardinality property]
   (object-exactly cardinality property nil))
  ([cardinality property class]
   (.getOWLObjectExactCardinality
    (owl-data-factory) cardinality
    (ensure-object-property property)
    (ensure-class class))))

(util/defmethodf exactly ::object object-exactly)

(defn object-oneof
  "Returns an OWL one of property restriction."
  [& individuals]
  (.getOWLObjectOneOf
   (owl-data-factory)
   (java.util.HashSet.
    (util/domap ensure-individual
                (flatten individuals)))))

(util/defmethodf oneof ::individual object-oneof)

(defnb object-has-value
  "Adds an OWL has-value restriction."
  [property individual]
  (.getOWLObjectHasValue (owl-data-factory)
                         (ensure-object-property property)
                         (ensure-individual individual)))

(util/defmethodf has-value ::object object-has-value)

(defn has-self
  "Returns an OWL has self restriction."
  [property]
  (.getOWLObjectHasSelf (owl-data-factory)
                        (ensure-object-property property)))
;; #+end_src

;; * OWL Class Complete



;; #+begin_src clojure
(def ^{:private true} owl-class-handlers
  {
   :subclass #'deprecated-add-subclass
   :sub #'add-subclass
   :super #'add-superclass
   :equivalent #'add-equivalent
   :disjoint #'add-disjoint
   :annotation #'add-annotation
   :haskey #'add-has-key
   :comment #'add-comment
   :label #'add-label
   })

(defn owl-class-explicit
  "Creates a class in the current ontology.
Frames is a map, keyed on the frame name, value a list of items (of other
lists) containing classes. This function has a rigid syntax, and the more
flexible 'owl-class' is normally preferred. However, this function should be
slightly faster.
"
  [o name frames]
  (let [class
        (ensure-class
         (if (string? name)
           (iri-for-name o name)
           name))]
    ;; add the class
    (add-axiom
     o
     (.getOWLDeclarationAxiom
      (owl-data-factory)
      class
      (p/as-annotations name)))
    ;; add an name annotation
    (add-a-name-annotation o class name)
    ;; apply the handlers to the frames
    (doseq [[k f] owl-class-handlers
            :when (get frames k)]
      (f o class (get frames k)))
    ;; return the class object
    class))

(defno owl-class
  "Creates a new class in the current ontology. See 'defclass' for
full details."
  [o name & frames]
  (owl-class-explicit
   o name
   (util/check-keys
    (util/hashify frames)
    (list*
     :ontology
     (keys owl-class-handlers)))))

(defentity defclass
  "Define a new class. Accepts a set number of frames, each marked
by a keyword :subclass, :equivalent, :annotation, :comment,
:label or :disjoint. Each frame can contain an item, a list of items or any
combination of the two. The class object is stored in a var called classname."
  'tawny.owl/owl-class)

(defn disjoint-classes
  "Makes all elements in list disjoint.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre [(sequential? list)
         (> (count list) 1)]}
  (let [classlist
        (util/domap
         (fn [x]
           (ensure-class x))
         list)]
    (add-axiom
     o
     (.getOWLDisjointClassesAxiom
      (owl-data-factory)
      (set classlist)
      (union-annotations classlist)))))

(defn equivalent-classes
  "Makes all elements in list equivalent.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre [(sequential? list)
         (> (count list) 1)]}
  (let [classlist
        (doall
         (map
          (fn [x]
            (ensure-class x))
          list))]
    (add-axiom
     o
     (.getOWLEquivalentClassesAxiom
      (owl-data-factory)
      (hset classlist)
      (union-annotations classlist)))))
;; #+end_src

;; * Individuals

;; (meta #'add-type)
;; #+begin_src clojure
(defnb2 add-type
  "Adds CLAZZES as a type to individual to current ontology
or ONTOLOGY if present."
  [o individual clazz]
  (add-axiom
   o
   (.getOWLClassAssertionAxiom
    (owl-data-factory)
    (ensure-class clazz)
    (p/as-entity individual)
    (p/as-annotations individual))))

(defnb2
  ^{:doc "Add FACTS to an INDIVIDUAL in the current ontology or
  ONTOLOGY if present. Facts are produced with `fact' and `fact-not'."}
  add-fact
  [o individual fact]
  (add-axiom
   o
   ((p/as-entity fact)
    individual
    (p/as-annotations fact))))

(defmulti get-fact #'guess-type-args)
(defmulti get-fact-not #'guess-type-args)

(defn fact
  "Returns a fact assertion a relation by PROPERTY which can be
either an object or data property toward either an individual or literal
TO. This is the same as the function `is'."
  [property to]
  (fn fact [from annotations]
    (get-fact property from to annotations)))

(def ^{:doc "Returns a fact assertion a relation by PROPERTY which can be
either an object or data property toward either an individual or literal
TO"}
  is
  fact)

(defn fact-not
  "Returns a fact asserting the lack of a relationship along PROPERTY
toward an either an individual or literal TO."
  [property to]
  (fn fact-not [from annotations]
    (get-fact-not property from to annotations)))

(defn object-get-fact
  "Returns an OWL Object property assertion axiom."
  [property from to annotations]
  {:pre [(say
          (t/obj-prop-exp? property)
          (t/individual? from)
          (t/individual? to))]}
  (.getOWLObjectPropertyAssertionAxiom
   (owl-data-factory)
   property from to annotations))

(util/defmethodf get-fact ::object object-get-fact)

(defn object-get-fact-not
  "Returns a negative OWL Object property assertion axiom."
  [property from to annotations]
  {:pre [(say
          (t/obj-prop-exp? property)
          (t/individual? from)
          (t/individual? to))]}
  (.getOWLNegativeObjectPropertyAssertionAxiom
   (owl-data-factory)
   property from to annotations))

(util/defmethodf get-fact-not ::object object-get-fact-not)

(defn
  add-same
  {:doc "Adds all arguments as the same individual to the current ontology
or to ONTOLOGY if present."
   :arglists '([ontology & individuals] [& individuals])}
  [o & individuals]
  (let [individuals (filter (comp not nil?) (flatten individuals))]
    (when individuals
      (add-axiom
       o
       (.getOWLSameIndividualAxiom
        (owl-data-factory)
        ^java.util.Set
        (set individuals)
        (union-annotations individuals))))))

(defn add-different
  {:doc "Adds all arguments as different individuals to the current
  ontology unless first arg is an ontology in which case this is used"}
  [o & individuals]
  (let [individuals (filter (comp not nil?) (flatten individuals))]
    (when individuals
      (add-axiom
       o
       (.getOWLDifferentIndividualsAxiom
        (owl-data-factory)
        (set individuals)
        (union-annotations individuals))))))

;; need to support all the different frames here...
;; need to use hashify -- need to convert to handlers
(def ^{:private true} individual-handlers
  {
   :type #'add-type
   :fact #'add-fact
   :same #'add-same
   :different #'add-different
   :annotation #'add-annotation
   :comment #'add-comment
   :label #'add-label
   })

(defno individual-explicit
  "Returns a new individual."
  [o name frames]
  (let [individual
        (ensure-individual
         (if (string? name)
           (iri-for-name o name)
           name))]
    ;; add the individual
    (when (.isNamed individual)
      (add-axiom o (.getOWLDeclarationAxiom
                    (owl-data-factory) individual)))
    ;; add a name annotation
    (add-a-name-annotation o individual name)
    ;; apply the handlers
    (doseq [[k f] individual-handlers
            :when (get frames k)]
      (f o individual (get frames k)))
    individual))

(defno individual
  [o name & frames]
  (individual-explicit
   o name
   (util/check-keys
    (util/hashify frames)
    (list* :ontology
           (keys individual-handlers)))))

(defentity defindividual
  "Declare a new individual"
  'tawny.owl/individual)

(defn anonymous-individual
  "Return a new anonymous individual"
  []
  (.getOWLAnonymousIndividual (owl-data-factory)))

;; #+end_src

;; * OWL Data

;; #+begin_src clojure
(load "owl_data")
;; #+end_src

;; * Grouping

;; Grouping capabilities. Tawny's define before use semantics is a bit of a pain
;; for mutually refering entities, so we provide some constructs for grouping
;; these, as well ~as-subclass~ and ~as-equivalents~ calls, which help us to
;; demonstrate syntactic intent.

;; #+begin_src clojure
(defn- var-get-maybe
  "Given a var return it's value, given a value return the value."
  [var-maybe]
  (if (var? var-maybe)
    (var-get var-maybe)
    var-maybe))

(defno as-disjoint
  {:doc "All entities are declared as disjoint. Entities may be
any structure and may also be a var. See also 'as-subclasses'."
   :arglists '([ontology & entities] [& entities])}
  [o & entities]
  (let [entities
        (map var-get-maybe (flatten entities))]
    (case
        (apply guess-type-args
               (map var-get-maybe
                    (flatten
                     entities)))
      ::class
      (disjoint-classes o entities)
      ::object-property
      (disjoint-properties o entities)
      ::data-property
      (disjoint-data-properties o entities)
      (throw (IllegalArgumentException.
              "Unable to determine the type of entities.")))))

(defno as-equivalent
  "Declare the properties or classes as disjoint."
  [o & entities]
  (let [entities
        (map var-get-maybe (flatten entities))]
    (case
        (apply guess-type-args
               (map var-get-maybe
                    (flatten
                     entities)))
      ::class
      (equivalent-classes o entities)
      ::object-property
      (equivalent-properties o entities)
      ::data-property
      (equivalent-data-properties o entities)
      (throw (IllegalArgumentException.
              "Unable to determine the type of entities.")))))

(defno as-inverse
  {:doc "Declare the two properties as inverse"
   :arglist '([ontology prop1 prop2] [prop1 prop2])}
  [o p1 p2]
  (add-inverse o
               (var-get-maybe p1)
               (var-get-maybe p2)))

(defno
  as-subclasses
  {:doc "All classes are given the superclass.
The first item may be an ontology, followed by options.

:disjoint also sets the class disjoint.
:cover also makes the subclasses cover the superclass."
   :arglists '([ontology superclass options & classes]
                 [superclass options & classes]
                   [superclass & classes])}
  [o superclass & rest]
  (let [options (set (take-while keyword? rest))
        subclasses
        (map
         var-get-maybe
         (flatten (drop-while keyword? rest)))]
    ;; first we deal with subclasses
    (add-subclass o superclass subclasses)
    (when
        (:disjoint options)
      (disjoint-classes o subclasses))
    (when (:cover options)
      (add-equivalent o superclass
                      (owl-or subclasses)))))

(defno
  as-disjoint-subclasses
  {:doc "Declare all subclasses as disjoint"}
  [o superclass & subclasses]
  (apply as-subclasses (list* o superclass :disjoint subclasses)))

;; hmmm, now how do we do the ontology thing here?
(defmacro declare-classes
  "Declares all the classes given in args. Any args including and following
the first keyword will be interpreted as frames for all the classes. Frame
args will be evaluated multiple times so should be side-effect free.

This is mostly useful for forward declarations.

See `defclassn' to define many classes with different frames.
"
  [& args]
  (let [nk (comp not keyword?)
        names (take-while nk args)
        frames (drop-while nk args)
        ]
    `(list
      ~@(map
         (fn [x#]
           `(defclass ~x# ~@frames))
         names))))

(defmacro defclassn
  "Defines many classes at once.

Each class and associated frames should be supplied as a vector.

See `declare-classes' where frames (or just default frames) are not needed.
"
  [& classes]
  `(list ~@(map
            (fn [x#]
              `(defclass ~@x#)) classes)))
;; #+end_src

;; * Predicate Functions

;; I think I have duplicated some of the functionality from the OWL API here, but
;; will check.

;; #+begin_src clojure
(defn- recurse-ontology-tree
  "Recurse the ontology tree starting from class and calling
f to get the neighbours."
  [o f entity]
  (loop [entities #{}
         explored #{}
         frontier (f o entity)]
    (if (empty? frontier)
      entities
      (let [v (first frontier)
            neighbours (seq (f o v))]
        (recur
         (conj entities v)
         (into explored neighbours)
         (into (rest frontier) (remove explored neighbours)))))))

;; predicates
(defno direct-superclasses
  "Returns the direct superclasses of name.
Name can be either a class or a string name. Returns a list of class
expressions."
  [^OWLOntology o name]
  (let [^OWLClass clz (ensure-class name)]
    ;; general Class expressions return empty
    (if (t/owl-class? clz)
      (EntitySearcher/getSuperClasses clz o)
      ())))

(defno superclasses
  "Return all superclasses of class.
class is not returned unless it is explicitly stated to be a
direct or indirect superclass of itself."
  [o class]
  (recurse-ontology-tree
   o direct-superclasses class))

(defno superclass?
  "Returns true if name is asserted to be a superclass."
  [o name superclass]
  (let [namecls (ensure-class name)
        superclasscls (ensure-class superclass)]
    (some #{superclasscls}
          (superclasses o name))))

(defno direct-subclasses
  "Returns the direct subclasses of name."
  [^OWLOntology o name]
  (let [clz (ensure-class name)]
    (if (t/owl-class? clz)
      (EntitySearcher/getSubClasses clz o) ())))

(defno subclasses
  "Return all subclasses of class."
  [o class]
  (recurse-ontology-tree
   o direct-subclasses class))

(defno subclass?
  "Returns true if name has subclass as a subclass."
  [o name subclass]
  (let [namecls (ensure-class name)
        subclasscls (ensure-class subclass)]
    (some
     #{subclasscls}
     (subclasses o name))))

(defno disjoint?
  "Returns t iff entities (classes or properties) are asserted to be
  disjoint."
  [^OWLOntology o a b]
  (let [type (guess-type-args a b)]
    (cond
     (isa? type ::class)
     (contains?
      (EntitySearcher/getDisjointClasses ^OWLClass a o)
      b)
     ;; works for either data or object properties
     (isa? type ::property)
     (contains?
      (EntitySearcher/getDisjointProperties ^OWLProperty a o)
      b)
     :default
     (throw
      (IllegalArgumentException.
       "Cannot determine disjoint for this form of entity")))))

(defno equivalent?
  "Returns t iff classes are asserted to be equivalent."
  [^OWLOntology o a b]
  (let [type (guess-type-args a b)]
    (cond
     (isa? type ::class)
     (contains?
      (EntitySearcher/getEquivalentClasses ^OWLClass a o) b)
     (isa? type ::property)
     (contains?
      (EntitySearcher/getEquivalentProperties ^OWLProperty a o)
      b)
     :default
     (throw
      (IllegalArgumentException.
       "Cannot determine equivalence for this type of entity")))))

(defno inverse?
  "Returns t iff properties are asserted to be inverse"
  [^OWLOntology o ^OWLObjectProperty p1 ^OWLObjectProperty p2]
  (contains?
   (EntitySearcher/getInverses p1 o) p2))

(defno direct-superproperties
  "Return all direct superproperties of property."
  [^OWLOntology o property]
  (EntitySearcher/getSuperProperties
   (ensure-property property) o))

(defno superproperties
  "Return all superproperties of a property."
  [o property]
  (recurse-ontology-tree
   o direct-superproperties
   (ensure-property property)))

(defno superproperty?
  "Return true if superproperty is a superproperty of property."
  [o property superproperty]
  (some #(.equals
          (ensure-property superproperty) %)
        (superproperties o property)))

(defno direct-subproperties
  "Returns all direct subproperties of property."
  [^OWLOntology o
   property]
  (EntitySearcher/getSubProperties
   (ensure-property property) o))

(defno subproperties
  "Returns all subproperties of property."
  [o property]
  (recurse-ontology-tree
   o direct-subproperties
   (ensure-property property)))

(defno subproperty?
  "Returns true if property is a subproperty of subproperty."
  [o property subproperty]
  (some #(.equals
          (ensure-property subproperty) %)
        (subproperties o property)))

(defno direct-instances
  "Return all direct instances of NAME class."
  [^OWLOntology o name]
  (let [clz (ensure-class name)]
    (if (t/owl-class? clz)
      (EntitySearcher/getIndividuals clz o) ())))
;; #+end_src

;; * Test Support

;; A few macros which are mostly useful for testing. Possibly I should pull these
;; out and put them somewhere else. The argument against is inertia.

;; #+begin_src clojure
(defmacro with-probe-entities
  {:doc
   "Evaluate BODY with a number of entities defined. Then delete these entities
  from the ontology. BINDINGS are a vector with similar to let. The first
  argument should evaluate to the ontology, or the current ontology will be
  used. Statements inside bindings are evaluated with the current-ontology set
  to ONTOLOGY. Entities added to ONTOLOGY are removed from ONTOLOGY; so if
  they are added to a different ontology explicitly, they will remain there
  after the completion of this form."
   :arglists '([bindings & body] [ontology bindings & body])
   }

  [& args]
  (let [o (take-while #(not (vector? %)) args)
        o (or (first o) `(get-current-ontology))
        rst (drop-while #(not (vector? %)) args)
        bindings (first rst)
        body (rest rst)
        ]
    (when-not (vector? bindings)
      (IllegalArgumentException. "with-probe-entities requires a vector"))
    (when-not (even? (count bindings))
      (IllegalArgumentException.
       "with-probe-entities requires an even number of forms in binding vector"))
    (cond
     (zero? (count bindings))
     `(do
        ~@body)
     (symbol? (bindings 0))
     `(let ~(subvec bindings 0 2)
       (with-probe-entities ~o
         ~(subvec bindings 2)
         ;; try block just so we can use finally
         (try
           ~@body
           (finally
             (tawny.owl/remove-entity ~o ~(bindings 0))))))
     :else
     (throw (IllegalArgumentException.
             "with-probe-entities only allows Symbols in bindings")))))


(defmacro with-probe-axioms
  "Evaluate the body with a number of axioms. Then
delete these axioms from the ontology.

This is mostly useful for test cases. Axioms can be added, consistency
or inconsistency can be checked then removed, leaving the ontology
effectively unchanged."
  [& args]
  (let [o (take-while #(not (vector? %)) args)
        o (or (first o) `(get-current-ontology))
        rst (drop-while #(not (vector? %)) args)
        bindings (first rst)
        body (rest rst)
        ]
    (when-not (vector? bindings)
      (IllegalArgumentException. "with-probe-axioms requires a vector"))
    (when-not (even? (count bindings))
      (IllegalArgumentException.
       "with-probe-axioms requires an even number of forms in binding vector"))
    (cond
     (zero? (count bindings))
     `(do ~@body)
     (symbol? (bindings 0))
     `(let ~(subvec bindings 0 2)
       (with-probe-axioms ~o
         ~(subvec bindings 2)
         ;; try block just so we can use finally
         (try
           ~@body
           (finally
             (tawny.owl/remove-axiom ~o ~(bindings 0))))))
     :else
     (throw (IllegalArgumentException.
             "with-probe-axioms only allows Symbols in bindings")))))

;; #+end_src

;; * More Grouping

;; Some limited usage macros for adding or removing prefixes from ~defclass~ forms.
;; In general, I am not convinced by these macros. It probably makes more sense
;; to make new macros with ~defentity~ which is fairly straight-forward now anyway.

;; #+begin_src clojure
;; add a prefix or suffix to contained defclass
(defn- alter-symbol-after-def-form
  "Searches for a defclass form, then changes the symbol by applying f."
  [f x]
  (cond
   (and (seq? x)
        (= (first x) 'defclass))
   `(defclass ~(f (second x))
      ~@(drop 2 x))
   :default
   x))

(defn- prefix-symbol
  "Add a prefix to a symbol and return a new symbol."
  [prefix sym]
  (symbol
   (str prefix (name sym))))

(defn- suffix-symbol
  "Add a suffix to a symbol and return a new symbol"
  [suffix sym]
  (symbol
   (str (name sym) suffix)))

(defn- alter-all-symbol-after-def-form
  "Walk over forms and applies function
f to the symbol after a defclass"
  [f x]
  (clojure.walk/postwalk
   (partial alter-symbol-after-def-form f)
   x))

(defmacro with-prefix
  "Adds a prefix to all defclass macros in scope.
This is a convenience macro and is lexically scoped."
  [prefix & body]
  (let [newbody
        (alter-all-symbol-after-def-form
         (partial prefix-symbol prefix)
         body)]
    `(list ~@newbody)))

(defmacro with-suffix
  "Adds a suffix to all defclass macros in scope.
This is a convenience macro and is lexically scoped."
  [suffix & body]
  (let [newbody
        (alter-all-symbol-after-def-form
         (partial suffix-symbol suffix)
         body)]
    `(list ~@newbody)))
;; #+end_src

;; * Refine

;; The idea of refining is to add new frames onto existing definitions, combined
;; with the various ~def~ forms which also create short-cut var references in
;; different name spaces.

;; Like the test forms above this is a candidate for moving out to a different
;; namespace, because it is distinctly non-core functionality.

;; #+begin_src clojure

(defno
  ^:private
  entity-class
  ([o en & _]
   (class en)))

(defmulti refine
  "Takes an existing definition, adds it to the current ontology, and then
adds more frames. owlentity is the OWLEntity to be refined, and frames are the
additional frames. The keys to the frames must be appropriate for the type of
entity. See 'owl-class' or 'object-property' for more details.

This is useful for two main reasons. First, to build class definitions in two
places and add frames in both of these places. For simple forward declaration
'declare-classes' is better. The second is where the same class needs to
appear in two ontologies, but with more axioms in the second. This can enable,
for example, building two interlocking ontologies with different OWL profiles.
"
  #'entity-class)

(util/defmethodf refine OWLClass owl-class)

(util/defmethodf refine OWLObjectProperty object-property)

(util/defmethodf refine OWLAnnotationProperty annotation-property)

(util/defmethodf refine OWLDataProperty datatype-property)

(util/defmethodf refine OWLDatatype datatype)

(util/defmethodf refine OWLIndividual individual)

(defmacro defrefine
  "Takes an existing definition, add more frames.

The first element should be a namespace qualified symbol. The
unqualified part of this will be used in the current namespace.

See also 'refine'
"
  [symb & args]
  (let [newsymbol#
        (symbol (name symb))
        ontsplit (extract-ontology-frame args)
        ont (:ontology ontsplit)
        frames (:frames ontsplit)
        ]
    `(def
       ~(with-meta newsymbol#
          (assoc (meta newsymbol#)
            :owl true))
       ~(if ont
          `(tawny.owl/refine
           ~ont
           ~symb ~@frames)
          `(tawny.owl/refine
           ~symb ~@frames)))))

(defmacro defcopy
  "Takes an existing definition from another namespace and copies it into the
current namespace with no changes in semantics. This can be useful for
convenience, where one namespace should contain all the OWLObjects of
another, or for forward declaration, where entities will be refined later.

This does not add the existing definition to the current ontology. In most
cases this will have been imported."
  [symb & args]
  (let [newsymbol#
        (symbol (name symb))]
    `(def
       ~(with-meta newsymbol#
          (assoc (meta newsymbol#)
            :owl true))
       (var-get (var ~symb)))))
;; #+end_src
