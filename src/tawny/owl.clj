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
(ns ^{:doc "Build ontologies in OWL."
      :author "Phillip Lord"}
  tawny.owl
  (:require
   [clojure.walk :only postwalk]
   [clojure.test :only is]
   [clojure.set]
   [clojure.java.io]
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

;; #+end_src


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

(defn- call-f-with-arity
  "Return a form which calls the function f, n times."
  [f n]
  (let [args
        (repeatedly n #(gensym "arg"))]
    (list
     (vec args)
     (concat f args))))

(defmacro defnwithfn
  "Define a new var like defn, but compose FUNCTION with BODY before
rather than just using BODY directly."
  [name function & body]
  (let [f (gensym)]
    `(let [vr# (defn ~name ~@body)]
       (alter-var-root
        vr#
        (fn [~f]
          ;; 0 to 20 arity
          (fn
          ~@(map
             #(call-f-with-arity
               [function f] %)
             (range 20))
          ;; and fall back
          ([a# b# c# d# e# f# g# h# i# j# k#
            l# m# n# o# p# q# r# s# & t#]
             (apply ~function ~f
                    a# b# c# d# e# f# g# h# i# j# k#
                    l# m# n# o# p# q# r# s# t#)))))
       vr#)))

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

(defmacro ^{:private true} dispatch-maybe
  "Dispatch with the default ontology if necessary and if it is present."
  [f & args]
  ;; the majority of this macro is duplicated in variadic
  ;; version of dispatch-ontology-maybe
  `(if
       (or (t/ontology?  ~(first args))
           (nil? ~(first args)))
     (~f ~@args)
     (do
       (tawny.util/run-hook tawny.owl/default-ontology-hook)
       (~f (tawny.owl/get-current-ontology-maybe) ~@args))))

(defn default-ontology-maybe
  "Invoke f ensuring the first argument is an ontology or nil.
The logic used is the same as default-ontology except that no error is
signalled if there is no current-ontology. The multi-arity function avoids
variadic calls most of the time."
  ([f a]
     (dispatch-maybe f a))
  ([f a b]
     (dispatch-maybe f a b))
  ([f a b c]
     (dispatch-maybe f a b c))
  ([f a b c d]
     (dispatch-maybe f a b c d))
  ([f a b c d e]
     (dispatch-maybe f a b c d e))
  ([f a b c d e fa]
     (dispatch-maybe f a b c d e fa))
  ([f a b c d e fa g]
     (dispatch-maybe f a b c d e fa g))
  ([f a b c d e fa g h]
     (dispatch-maybe f a b c d e fa g h))
  ([f a b c d e fa g h i]
     (dispatch-maybe f a b c d e fa g h i))
  ([f a b c d e fa g h i j]
     (dispatch-maybe f a b c d e fa g h i j))
  ([f a b c d e fa g h i j & args]
     (if (or
          (t/ontology? a)
          (nil? a))
       (apply f a b c d e fa g h i j args)
       (apply default-ontology-base-dispatcher
              get-current-ontology-maybe
              f a b c d e fa g h i j args))))

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
       (apply default-ontology-base-dispatcher
              get-current-ontology
              f a b c d e fa g h i j args))))
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
(defn- broadcast-ontology-int
  "Implements the broadcast function for up to six arguments. First argument
is an ontology, the last is th function that we are broadcasting to, and all
the other arguments are arguments to be passed. At this point o may be either
an ontology or nil, depending on what calls this, and a-f are definately not
lists. There was a good reason for putting fnc at the end of the argument
list, but I cannot remember what it was."
  ;; at this point o is definately an ontology and a-f are definately not lists
  ([o a fnc]
   (fnc o a))
  ([o a b fnc]
   ;; dispense with the list for this case when we don't need it and also
   ;; because it will allow functions operating on the return value of this to
   ;; also avoid the full flattening broadcast support.
   (fnc o a b))
  ([o a b c fnc]
     (list
      (fnc o a b)
      (fnc o a c)))
  ([o a b c d fnc]
     (list
      (fnc o a b)
      (fnc o a c)
      (fnc o a d)))
  ([o a b c d e fnc]
     (list
      (fnc o a b)
      (fnc o a c)
      (fnc o a d)
      (fnc o a e)))
  ([o a b c d e f fnc]
     (list
      (fnc o a b)
      (fnc o a c)
      (fnc o a d)
      (fnc o a e)
      (fnc o a f)))
  ([o a b c d e f g fnc]
     (list
      (fnc o a b)
      (fnc o a c)
      (fnc o a d)
      (fnc o a e)
      (fnc o a f)
      (fnc o a g))))

(defn broadcast-ontology-full
  "Given a function which expects an ontology and two other arguments, ensure
that the first argument is an ontology (see default-ontology for details),
then f repeatedly against the second args and all subsequent arguments
flattened. Where possible, we avoid using this function for the micro-optimisd
broadcast-ontology."
  [f & args]
  (apply default-ontology
         (fn [o & narg]
           (doall
            (map (partial f o (first narg))
                 (flatten
                  (rest narg)))))
         args))

(defmacro ^{:private true}
  if-not-sequential
  "If all seqs are not sequential. This is a micro-optimisation, as use of
every? requires a list at run time when we have an list of arguments." 
  [seqs & rest]
  `(if (and
        ~@(map
           (fn [seq]
             `(not (sequential? ~seq)))
           ;; reverse the seqs because the last argument is often a list, for
           ;; calls from the named entity functions, such as owl-class. So, we
           ;; can break early and fail fast in these cases.
           (reverse seqs)))
     ~@rest))

(defn broadcast-ontology
  "Given a function, fnc, and args ensure that the first arg is an ontology
using default-ontology, and then broadcast the rest, so that the fnc is called
with the first and second args, first and third args and so on. This function
is micro-optimised to avoid use of variadic method calls or list operations."
  ([fnc a b]
     (if-not-sequential
      [a b]
      (default-ontology
        broadcast-ontology-int
        a b fnc)
      (broadcast-ontology-full fnc a b)))
  ([fnc a b c]
     (if-not-sequential
      [a b c]
      (default-ontology
        broadcast-ontology-int a b c fnc)
      (broadcast-ontology-full fnc a b c)))
  ([fnc a b c d]
     (if-not-sequential
      [a b c d]
      (default-ontology
        broadcast-ontology-int a b c d fnc)
      (broadcast-ontology-full fnc a b c d)))
  ([fnc a b c d e]
     (if-not-sequential
      [a b c d e]
      (default-ontology
        broadcast-ontology-int a b c d e fnc)
      (broadcast-ontology-full fnc a b c d e)))
  ([fnc a b c d e f]
     (if-not-sequential
      [a b c d e f]
      (default-ontology
        broadcast-ontology-int a b c d e f fnc)
      (broadcast-ontology-full fnc a b c e f)))
  ([fnc a b c d e f g]
     (if-not-sequential
      [a b c d e f g]
      (default-ontology
        broadcast-ontology-int a b c d e f g fnc)
      (broadcast-ontology-full fnc a b c d e f g)))
  ([fnc a b c d e f g & args]
     (apply broadcast-ontology-full
            fnc a b c d e f g args)))

(defn- broadcast-ontology-maybe-full
  "Like broadcast-ontology-maybe-full but does not signal an error if there is no current
ontology."
  [f & args]
  (apply default-ontology-maybe
         (fn broadcast-ontology-maybe [o & narg]
           (doall
            (map (partial f o (first narg))
                 (flatten
                  (rest narg)))))
         args))

(defn broadcast-ontology-maybe
  "Like broadcast-ontology but does not signal an error where there is no
default ontology."
  ([fnc a b]
     (if-not-sequential
      [a b]
      (default-ontology-maybe
        broadcast-ontology-int
        a b fnc)
      (broadcast-ontology-maybe-full fnc a b)))
  ([fnc a b c]
     (if-not-sequential
      [a b c]
      (default-ontology-maybe
        broadcast-ontology-int a b c fnc)
      (broadcast-ontology-maybe-full fnc a b c)))
  ([fnc a b c d]
     (if-not-sequential
      [a b c d]
      (default-ontology-maybe
        broadcast-ontology-int a b c d fnc)
      (broadcast-ontology-maybe-full fnc a b c d)))
  ([fnc a b c d e]
     (if-not-sequential
      [a b c d e]
      (default-ontology-maybe
        broadcast-ontology-int a b c d e fnc)
      (broadcast-ontology-maybe-full fnc a b c d e)))
  ([fnc a b c d e f]
     (if-not-sequential
      [a b c d e f]
      (default-ontology-maybe
        broadcast-ontology-int a b c d e f fnc)
      (broadcast-ontology-maybe-full fnc a b c d e f)))
  ([fnc a b c d e f g]
     (if-not-sequential
      [a b c d e f g]
      (default-ontology-maybe
        broadcast-ontology-int a b c d e f g fnc)
      (broadcast-ontology-maybe-full fnc a b c d e f g)))
  ([fnc a b c d e f g & args]
     (apply broadcast-ontology-maybe-full
            fnc a b c d e f g args)))


;; #+end_src

;; * Function def forms

;; These ~def~ macros enable definition of functions with support for
;; broadcasting and default ontology functionality. The difference is the 4 and
;; 5th letter:

;;  - "d" definately requires the default ontology
;;  - "m" maybe requires the default ontology
;;  - "b" broadcasting

;; The reason for the "maybe" is that for some functions, it depends on the
;; arguments whether an ontology is required or not. So:

;; #+BEGIN_EXAMPLE
;; (owl-class "a")
;; #+END_EXAMPLE

;; definately requires an ontology since we need to add ~a~ to the it.

;; #+BEGIN_EXAMPLE
;; (owl-some r A)
;; #+END_EXAMPLE

;; Does not if ~r~ and ~A~ are object property and class objects respectively.
;; However,

;; #+BEGIN_EXAMPLE
;; (owl-some r "a")
;; #+END_EXAMPLE

;; will do since it is equivalent to 

;; #+BEGIN_EXAMPLE
;; (owl-some r (owl-class "a"))
;; #+END_EXAMPLE

;; In this case, ~owl-some~ calls a "definately" ontology function anyway, and it
;; is this that crashes. Tawny uses a "crash late" strategy -- we should only
;; require an ontology argument when it is definately needed.

;; One disadvantage of these ~def~ forms is that they make the stack traces a lot
;; harder to read, since their bodies are actually transformed into numbered
;; functions. I am not sure how to fix this.

;; #+begin_src clojure

(defmacro defdontfn
  "Like defn, but automatically adds the current-ontology to the args
if the first arg is not an ontology. Throws an IllegalStateException
if there is no current ontology.

The 'd' stands for definitely."
  [name & body]
  `(defnwithfn ~name #'default-ontology
     ~@body))

(defmacro defmontfn
  "Like defn, but automatically adds the current ontology or nil to the args
  if the first arg is not an ontology.

The 'm' stands for maybe."
  [name & body]
  `(defnwithfn ~name #'default-ontology-maybe
     ~@body))

(defmacro defbdontfn
  "Like the defn and defdontfn, but broadcasts. That is it expects a three arg
function, f with ontology, x and y, but defines a new function which takes
ontology, x and rest, returning a list which is f applied to ontology, x and
the flattened elements of rest.

Uses the default ontology if not supplied and throws an IllegalStateException
  if this is not set."
  [name & body]
  `(defnwithfn ~name #'broadcast-ontology
     ~@body))

(defmacro defbmontfn
  "Like defbdontfn, but also accepts nil as either the passed or default ontology."
  [name & body]
  `(defnwithfn ~name #'broadcast-ontology-maybe
     ~@body))


;; #+end_src

;; * Axiom mainpulation

;; Just some utility functions for adding or removing axioms.

;; #+begin_src clojure
(defdontfn add-axiom
  "Adds an axiom from the given ontology, or the current one."
  [^OWLOntology o  ^OWLAxiom axiom]
  (.applyChange (owl-ontology-manager)
                (AddAxiom. o axiom))
  axiom)

(defdontfn remove-axiom
  "Removes a list of axioms from the given ontology, or the current one."
  [o & axiom-list]
  (doall
   (map (fn [axiom]
          (.applyChange (owl-ontology-manager)
                        (RemoveAxiom. o axiom)))
        (flatten axiom-list))))

(defdontfn remove-entity
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
(defdontfn ontology-options
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

(defdontfn iri-for-name
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
(defbdontfn add-a-simple-annotation
  "Adds an annotation to a named objecqt."
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
    (add-a-simple-annotation o named-entity (tawny-name name))))
;; #+end_src

;; Now we add support for annotating ontologies.

;; #+begin_src clojure
(defbdontfn add-an-ontology-annotation
  "Adds an annotation to an ontology."
  [o o annotation]
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
(defdontfn add-annotation
  {:doc "Add an annotation in the ontology to either the named-entity
or the ontology. Broadcasts over annotations."
   :arglists '([ontology named-entity & annotations]
                 [named-entity & annotations]
                   [ontology & annotations][annotations])}
  [o & args]
  (if (t/named? (first args))
    (add-a-simple-annotation
     o (first args) (rest args))
    (add-an-ontology-annotation
     ;; okay, so this is wierd, but broadcasting requires a three arg
     ;; function, first being an ontology.
     o o args)))

(defn- ^OWLAnnotationProperty ensure-annotation-property
  "Ensures that 'property' is an annotation property,
converting it from a string or IRI if necessary."
  [o property]
  (cond
   (t/ann-prop? property)
   property
   (t/iri? property)
   (.getOWLAnnotationProperty
    (owl-data-factory) property)
   (instance? String property)
   (ensure-annotation-property
    o
    (iri-for-name o property))
   :default
   (throw (IllegalArgumentException.
           (format "Expecting an OWL annotation property: %s" property)))))

(defmontfn annotation
  "Creates a new annotation. If literal is a string it is interpreted as a
String in English. Otherwise, it can be any annotation value or object which
can be co-erced to an IRI"
  ([o annotation-property literal]
     (cond
      (instance? String literal)
      (annotation o annotation-property literal "en")
      (t/ann-val? literal)
      (.getOWLAnnotation
       (owl-data-factory)
       (ensure-annotation-property o annotation-property)
       literal)
      (iriable? literal)
      (annotation o annotation-property (p/as-iri literal))
      :default
      (throw (IllegalArgumentException.
              "annotation takes a String, OWLAnnotationValue or an object with an IRI."))))
  ([o annotation-property ^String literal ^String language]
     (annotation o annotation-property
                 (.getOWLLiteral (owl-data-factory) literal language))))

(defbdontfn add-super-annotation
  "Adds a set of superproperties to the given subproperty."
  [o subproperty superproperty]
  (add-axiom
   o
   (.getOWLSubAnnotationPropertyOfAxiom
    (owl-data-factory)
    subproperty
    (ensure-annotation-property o superproperty)
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
  (fn annotator-pattern [& args]
    (apply
     default-ontology-maybe
      (fn
        ([o literal]
           (annotation o annotation-property literal))
        ([o literal language]
           (annotation o annotation-property literal language)))
      args)))

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

(defbmontfn add-label
  "Add labels to the named entities."
  [o named-entity label]
  (add-annotation
   o
   named-entity
   [(tawny.owl/label o label)]))

(defbmontfn add-comment
  "Add comments to the named entities."
  [o named-entity comment]
  (add-annotation o named-entity
                  [(owl-comment o comment)]))
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

(defdontfn annotation-property-explicit
  "Add this annotation property to the ontology"
  [o name frames]
  (let [property (ensure-annotation-property o name)]
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

(defdontfn annotation-property
  {:doc "Creates a new annotation property."
   :arglists '([ontology name & frames] [name & frames])}
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
  [frames]
  (if (= :ontology (first frames))
    {:ontology (second frames)
     :frames (nthrest frames 2)}
    {:ontology nil
     :frames frames}))

(defn- entity-generator [entity frames entity-function]
  (let [ontsplit (extract-ontology-frame frames)
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
  [name docstring entity-function]
  `(defmacro
     ~name
     ~docstring
     [entity# & frames#]
     (#'tawny.owl/entity-generator entity# frames# ~entity-function)))
;; #+end_src

;; * Last bit of annotation

;; And, finally, we complete the annotation frame.

;; #+begin_src clojure

(defentity defaproperty
  "Defines a new annotation property in the current ontology.
See 'defclass' for more details on the syntax"
  'tawny.owl/annotation-property)

(comment
  (defmacro defaproperty
    "Defines a new annotation property in the current ontology.
See 'defclass' for more details on the syntax."
    [property & frames]
    (let [ontsplit (extract-ontology-frame frames)
          ont (:ontology ontsplit)
          frames (:frames ontsplit)]
      `(let [property-name# (name '~property)
             property#
             (tawny.owl/annotation-property
              ~ont
              property-name#
              ~@frames)]
         (intern-owl ~property property#)))))

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
    (add-an-ontology-annotation
     o o (tawny-name n))))

(defn- add-ontology-annotation-handler [o annotations]
  (add-an-ontology-annotation o o annotations))

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
    (add-annotation o (owl-comment o s))))

(defn- add-see-also
  "Adds a see also annotation to the ontology"
  [o s]
  (if s
    (add-annotation o (see-also o s))))

(defn- add-version-info
  "Adds a version info annotation to the ontology."
  [o v]
  (if v
    ;; ontology annotation is a default ontology function, so need to pass
    ;; ontology twice even if this makes little sense!
    (add-annotation o (version-info o v))))

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
   :name #'add-an-ontology-name
   :seealso #'add-see-also
   :comment #'add-ontology-comment
   :versioninfo #'add-version-info
   ;; these two are specially dealt with and are broadcast
   :annotation #'add-ontology-annotation-handler
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
                      (str "Frame " k " can only be referenced once - try broadcasting"))))
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
                                    (:name options))})
        iri (iri (get options :iri
                             (str
                              (java.util.UUID/randomUUID)
                              (if-let [name
                                       (get options :name)]
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
(defmacro defontology
  "Define a new ontology with `name'.

The following keys must be supplied.
:iri -- the IRI for the new ontology
:prefix -- the prefix used in the serialised version of the ontology
"
  [name & body]
  `(do
     (let [ontology# (ontology :name ~(clojure.core/name name) ~@body)
           var#
           (tawny.owl/intern-owl ~name ontology#)]
       (tawny.owl/ontology-to-namespace ontology#)
       var#)))

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

(defdontfn entity-for-iri
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

(defdontfn entity-for-string
  "Returns the OWLObject for a given string.
See 'entity-for-iri' for more details. Attempts both ontology specific iri to name
conversion, and direct use of string as an IRI."
  [o string]
  (or (entity-for-iri o (iri-for-name o string))
      ;; string name somewhere?
      (entity-for-iri o (iri string))))

(defdontfn ^String get-prefix
  "Returns the prefix for the given ontology, or the current ontology if none
is given."
  [^OWLOntology o]
  ;; my assumption here is that there will only ever be one prefix for a given
  ;; ontology. If not, it's all going to go wrong.
  (.getDefaultPrefix
   (.asPrefixOWLOntologyFormat
    (.getOntologyFormat (owl-ontology-manager)
                        o))))

(defdontfn save-ontology
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

(defmontfn guess-type
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
  [o entity]
  (let [entity (p/as-entity entity)]
    (cond
     ;; it's a collection -- find the first entity
     (coll? entity)
     (some (partial guess-type o) entity)
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
     ;; up to this point, o can be nil -- after this point, we need to know
     ;; the ontology we are searching in.
     ;; if an IRI, see if it is the current ontology
     (t/iri? entity)
     (when o
       (guess-type o (entity-for-iri o entity)))
     ;; keyword -- these are builtin OWL2Datatypes
     (and (keyword? entity)
          (get owl2datatypes entity))
     ::data
     ;; string name in current ontology?
     (string? entity)
     (when o
       (guess-type o (entity-for-string o entity)))
     ;; owl individuals tell us nothing, cause we still don't know!
     (t/individual? entity)
     nil
     (number? entity)
     nil
     ;; if we get nil, carry on, because we may be able to determine the type
     ;; from a later argument.
     (nil? entity)
     nil
     ;; probably it's something crazy here.
     :default
     (throw (IllegalArgumentException.
             (str "Cannot guess this type:" entity))))))

(defmontfn guess-individual-literal
  [o entity]
  (let [entity (p/as-entity entity)]
    (cond
     (coll? entity)
     (some (partial guess-individual-literal o) entity)
     (t/individual? entity)
     ::individual
     (t/literal? entity)
     ::literal
     (t/iri? entity)
     (guess-individual-literal o
                               (entity-for-iri o entity))
     (string? entity)
     (if-let [owlentity (entity-for-string o entity)]
       (guess-individual-literal
        o owlentity)
       ::literal)
     (number? entity)
     ::literal
     (or (= true entity)
         (= false entity))
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
  [o prop]
  (let [prop (p/as-entity prop)]
    (cond
     (fn? prop)
     (ensure-object-property o (prop))
     (t/obj-prop-exp? prop)
     prop
     (t/iri? prop)
     (.getOWLObjectProperty (owl-data-factory) prop)
     (string? prop)
     (ensure-object-property o (iri-for-name o prop))
     :default
     (throw (IllegalArgumentException.
             (str "Expecting an object property. Got: " prop))))))

(defn- ensure-class-except [clz]
  (throw (IllegalArgumentException.
          (str "Expecting a class. Got: " clz))))

(defn- ^OWLClass ensure-class
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  [o clz]
  ;; convert to entity if necessary
  (let [clz (p/as-entity clz)]
    (cond
     (t/class-exp? clz)
     clz
     (t/iri? clz)
     (.getOWLClass (owl-data-factory) clz)
     (string? clz)
     (ensure-class o (iri-for-name o clz))
     (fn? clz)
     (try
       (ensure-class o (clz))
       (catch clojure.lang.ArityException e
         (ensure-class-except clz)))
     true (ensure-class-except clz))))

(defn-
  ^OWLDataProperty ensure-data-property
  "Ensures that 'property' is an data property,
converting it from a string or IRI if necessary."
  [o property]
  (let [property (p/as-entity property)]
    (cond
     (t/data-prop-exp? property)
     property
     (t/iri? property)
     (.getOWLDataProperty
      (owl-data-factory) property)
     (instance? String property)
     (ensure-data-property o
                           (iri-for-name o property))
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
  [o prop]
  (let [prop (p/as-entity prop)
        type
        (or
          ;; guess the type -- if we can't then object-property it's because
          ;; we don't know and not because it's illegal
          (guess-type o prop)
          ::object-property)]
    (case type
      ::object-property
      (ensure-object-property o prop)
      ::data-property
      (ensure-data-property o prop))))

(defn- ^OWLDatatype ensure-datatype
  "Ensure that 'datatype' is an OWLDatatype. Will convert from an keyword for
  builtin datatypes."
  [o datatype]
  (let [datatype (p/as-entity datatype)]
    (cond
     (t/data-type? datatype)
     datatype
     (instance? org.semanticweb.owlapi.vocab.OWL2Datatype datatype)
     datatype
     (keyword? datatype)
     (if-let [d (get owl2datatypes datatype)]
       (ensure-datatype o d)
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
  [o data-range]
  (cond
   (t/data-range? data-range)
   data-range
   :default
   (ensure-datatype o data-range)))

(defn- ^OWLIndividual ensure-individual
  "Returns an INDIVIDUAL.
If INDIVIDUAL is an OWLIndividual return individual, else
interpret this as a string and create a new OWLIndividual."
  [o individual]
  (let [individual (p/as-entity individual)]
    (cond
      (t/individual? individual)
      individual
      (t/iri? individual)
      (.getOWLNamedIndividual (owl-data-factory)
                              individual)
      (string? individual)
      (ensure-individual o (iri-for-name o individual))
      :default
      (throw (IllegalArgumentException.
              (str "Expecting an Individual. Got: " individual))))))
;; #+end_src

;; * OWL Class

;; We start ~OWLClass~ support rather randomly at this point.

;; #+begin_src clojure
(defbdontfn
  add-superclass
  {:doc "Adds one or more superclasses to name in ontology."
   :arglists '([name & superclass] [ontology name & superclass])}
  [o name superclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              (owl-data-factory)
              (ensure-class o name)
              (ensure-class o superclass)
              (p/as-annotations superclass))))

(defbdontfn
  add-subclass
  {:doc "Adds one or more subclasses to name in ontology."
   :arglists '([name & subclass] [ontology name & subclass])}
  [o name subclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              (owl-data-factory)
              (ensure-class o subclass)
              (ensure-class o name)
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

(defbdontfn add-equivalent
  {:doc "Adds an equivalent axiom to the ontology."
   :arglists '([name equivalent] [ontology name equivalent])
   }
  [o name equivalent]
  (add-axiom o
             (.getOWLEquivalentClassesAxiom
              (owl-data-factory)
              (ensure-class o name)
              (ensure-class o equivalent)
              (p/as-annotations equivalent))))

(defbdontfn add-disjoint
  {:doc "Adds a disjoint axiom to the ontology."
   :arglists '([name disjoint] [ontology name disjoint])}
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointClassesAxiom
    (owl-data-factory)
    (hash-set (ensure-class o name)
              (ensure-class o disjoint))
    (p/as-annotations disjoint))))

(defdontfn add-disjoint-union
  "Adds a disjoint union axiom to all subclasses."
  [o clazz subclasses]
  (let [ensured-subclasses
        (util/domap #(ensure-class o %) subclasses)
        ]
    (list
     (add-axiom o
                (.getOWLDisjointUnionAxiom
                 (owl-data-factory)
                 (ensure-class o clazz)
                 (set
                  (map
                   (partial ensure-class o)
                   subclasses))
                 (union-annotations subclasses))))))

(defdontfn add-class
  "Adds a class to the ontology."
  [o name]
  (add-axiom
   o
   (.getOWLDeclarationAxiom
    (owl-data-factory)
    (ensure-class o name)
    (p/as-annotations name))))

;; a class can have only a single haskey, so ain't no point broadcasting this.
(defdontfn add-has-key
  "Adds a has-key to the class."
  [o class propertylist]
  ;; nil or empty list safe
  (if (seq propertylist)
    (let [type (guess-type o propertylist)
          propertylist
          (cond
           (isa? type ::object)
           (map (partial ensure-object-property o) propertylist)
           (isa? type ::data)
           (map (partial ensure-data-property o) propertylist)
           :default
           (throw
            (IllegalArgumentException.
             "Unable to determine type of property")))]
      (add-axiom o
                 (.getOWLHasKeyAxiom
                  (owl-data-factory)
                  (ensure-class o class)
                  (set propertylist)
                  (p/as-annotations propertylist))))))
;; #+end_src

;; We end rather early here, and should probably bring the rest of the OWLClass
;; code in.

;; * Object Properties

;; And now we move onto ~OWLObjectProperty~ support.

;; #+begin_src clojure
(defbdontfn add-domain
  "Adds all the entities in domainlist as domains to a property."
  [o property domain]
  (add-axiom o
             (.getOWLObjectPropertyDomainAxiom
              (owl-data-factory)
              (ensure-object-property o property)
              (ensure-class o domain)
              (p/as-annotations domain))))

(defbdontfn add-range
  "Adds all the entities in rangelist as range to a property."
  [o property range]
  (add-axiom o
             (.getOWLObjectPropertyRangeAxiom
              (owl-data-factory)
              (ensure-object-property o property)
              (ensure-class o range)
              (p/as-annotations range))))

(defbdontfn add-inverse
  "Adds all the entities in inverselist as inverses to a property."
  [o property inverse]
  (add-axiom o
             (.getOWLInverseObjectPropertiesAxiom
              (owl-data-factory)
              (ensure-object-property o property)
              (ensure-object-property o inverse)
              (p/as-annotations inverse))))

(defmontfn inverse
  "Creates an object inverse of expression."
  [o property]
  (.getOWLObjectInverseOf
   (owl-data-factory)
   (ensure-object-property o property)))

(defbdontfn add-superproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property superproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              (owl-data-factory)
              (ensure-object-property o property)
              (ensure-object-property o superproperty)
              (p/as-annotations superproperty))))

(defbdontfn add-subproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property subproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              (owl-data-factory)
              (ensure-object-property o subproperty)
              (ensure-object-property o property)
              (p/as-annotations subproperty))))

(def ^{:deprecated "1.1"
       :doc "This is the same as add-superproperty but marked as deprecated
and used as the handler for :subproperty."
       } deprecated-add-superproperty
  add-superproperty)

;; broadcasts specially
(defdontfn add-subchain
  "Adds a property chain to property."
  [o property subpropertylist]
  (when subpropertylist
    (let [property (ensure-object-property o property)
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
           (map (partial ensure-object-property o) properties)
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

(defbdontfn add-equivalent-property
  "Adds a equivalent data properties axiom."
  [o property equivalent]
  (add-axiom
   o (.getOWLEquivalentObjectPropertiesAxiom
      (owl-data-factory)
      (ensure-object-property o property)
      (ensure-object-property o equivalent)
      (p/as-annotations equivalent))))

(defdontfn equivalent-properties
  "Adds properties as equivalent to the ontology."
  [o properties]
  (let [properties
        (map (partial
               ensure-object-property o) properties)]
    (add-axiom
     o (.getOWLEquivalentObjectPropertiesAxiom
        (owl-data-factory)
        (hset properties)
        (union-annotations properties)))))

(defbdontfn add-disjoint-property
  "Adds a disjoint property axiom to the ontology"
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointObjectPropertiesAxiom
    (owl-data-factory)
    (hash-set
     (ensure-object-property o name)
     (ensure-object-property o disjoint))
    (p/as-annotations disjoint))))

(defdontfn disjoint-properties
  "Make all the properties in PROPERTIES disjoint."
  [o properties]
  (let [properties
        (doall
         (map (partial ensure-object-property o) properties))]
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

(defbdontfn add-characteristics
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
      (ensure-object-property o property)
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
(defdontfn object-property-explicit
  "Returns an object-property. This requires an hash with a list
value for each frame."
  [o name frames]
  (let [o (or (first (get frames :ontology))
              o)
        property (ensure-object-property o name)]
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

(defdontfn object-property
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

(comment
  (defmacro defoproperty
    "Defines a new object property in the current ontology."
    [property & frames]
    `(let [property-name# (name '~property)
           property# (tawny.owl/object-property property-name# ~@frames)]
       (intern-owl ~property property#))))


;; #+end_src

;; * Random guess-type stuff

;; Why is this not earlier?

;; #+begin_src clojure

(defmontfn
  guess-type-args
  {:doc  "Broadcasting version of guess-type"
   :private true}
  [o & args]
  (guess-type o args))

(defmontfn guess-individual-literal-args
  {:doc "Broadcasting version of guess-individual-literal"
   :private true}
  [o & args]
  (guess-individual-literal o args))
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

(defmontfn owl-not
  "Returns a complement of restriction or negative property assertion axiom."
  ([o entity]
     (owl-not-one o entity))
  ([o property entity]
     (fact-not o property entity)))

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

(defbmontfn object-some
  {:doc "Returns an OWL some values from restriction."
   :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectSomeValuesFrom
   (owl-data-factory)
   (ensure-object-property o property)
   (ensure-class o class)))

;; use add method because we want object-some to have independent life!
(defmethod owl-some ::object [& rest]
  (apply object-some rest))

(defbmontfn object-only
  {:doc "Returns an OWL all values from restriction."
   :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectAllValuesFrom
   (owl-data-factory)
   (ensure-object-property o property)
   (ensure-class o class)))

(defmethod only ::object [& rest]
  (apply object-only rest))

;; union, intersection
(defmontfn object-and
  "Returns an OWL intersection of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-and must have at least two classes")))

    (.getOWLObjectIntersectionOf
     (owl-data-factory)
     (java.util.HashSet.
      (util/domap
       #(ensure-class o %)
       ;; flatten list for things like owl-some which return lists
       classes)))))

;; add to multi method
(defmethod owl-and ::object [& rest]
  (apply object-and rest))

(defmontfn object-or
  "Returns an OWL union of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-or must have at least two classes")))

    (.getOWLObjectUnionOf
     (owl-data-factory)
     (java.util.HashSet.
      (util/domap #(ensure-class o %)
                  (flatten classes))))))

(defmethod owl-or ::object [& rest]
  (apply object-or rest))

;; lots of restrictions return a list which can be of size one. so all these
;; functions take a list but ensure that it is of size one.
(defmontfn object-not
  "Returns an OWL complement of restriction."
  [o & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectComplementOf
   (owl-data-factory)
   (ensure-class o (first (flatten class)))))

(defmethod owl-not-one ::object [& rest]
  (apply object-not rest))

(defmontfn object-some-only
  "Returns an restriction combines the OWL some values from and
all values from restrictions."
  [o property & classes]
  (list
   (apply
    object-some
    o
    property classes)
   (object-only o property
                (apply object-or o classes))))

(defmethod some-only ::object [& rest]
  (apply object-some-only rest))

;; cardinality
(defmacro with-optional-class
  "Calls form as is, or adds n to it iff n is not nil.
n is evaluated only once, so can have side effects."
  [o n form]
  (let [nn (gensym "n")]
    `(let [~nn (first (flatten ~n))]
       (if (nil? ~nn)
         ~form
         ~(concat
           form
           `((ensure-class ~o ~nn)))))))


(defmontfn object-at-least
  "Returns an OWL at-least cardinality restriction."
  [o cardinality property & class]
  {:pre [(> 2
            (count (flatten class)))]}
  ;; we only take a single class here, but it could be in any form because of
  ;; the broadcasting functions
  (with-optional-class
    o class
    (.getOWLObjectMinCardinality
     (owl-data-factory) cardinality
     (ensure-object-property o property))))

(defmethod at-least ::object [& rest]
  (apply object-at-least rest))

(defmontfn object-at-most
  "Returns an OWL at-most cardinality restriction."
  [o cardinality property & class]
  {:pre [(> 2
            (count (flatten class)))]}
  (with-optional-class o class
    (.getOWLObjectMaxCardinality
     (owl-data-factory) cardinality
     (ensure-object-property o property))))

(defmethod at-most ::object [& rest]
  (apply object-at-most rest))

(defmontfn object-exactly
  "Returns an OWL exact cardinality restriction."
  [o cardinality property & class]
  {:pre [(and
          (number? cardinality)
          (> 2
             (count (flatten class))))]}
  (with-optional-class o class
    (.getOWLObjectExactCardinality
     (owl-data-factory) cardinality
     (ensure-object-property o property))))

(defmethod exactly ::object [& rest]
  (apply object-exactly rest))

(defmontfn object-oneof
  "Returns an OWL one of property restriction."
  [o & individuals]
  (.getOWLObjectOneOf
   (owl-data-factory)
   (java.util.HashSet.
    (util/domap (partial ensure-individual o)
                (flatten individuals)))))

(defmethod oneof ::individual [& rest]
  (apply object-oneof rest))

(defmontfn object-has-value
  "Adds an OWL has-value restriction."
  [o property individual]
  (.getOWLObjectHasValue (owl-data-factory)
                         (ensure-object-property o property)
                         (ensure-individual o individual)))

(defmethod has-value ::object [& rest]
  (apply object-has-value rest))

(defmontfn has-self
  "Returns an OWL has self restriction."
  [o property]
  (.getOWLObjectHasSelf (owl-data-factory)
                        (ensure-object-property o property)))
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

(defdontfn owl-class-explicit
  "Creates a class in the current ontology.
Frames is a map, keyed on the frame name, value a list of items (of other
lists) containing classes. This function has a rigid syntax, and the more
flexible 'owl-class' is normally preferred. However, this function should be
slightly faster.
"
  [o name frames]
  (let [class (ensure-class o name)]
    ;; add the class
    (add-class o class)
    ;; add an name annotation
    (add-a-name-annotation o class name)
    ;; apply the handlers to the frames
    (doseq [[k f] owl-class-handlers
            :when (get frames k)]
      (f o class (get frames k)))
    ;; return the class object
    class))

(defdontfn owl-class
  "Creates a new class in the current ontology. See 'defclass' for
full details."
  [o name & frames]
  (owl-class-explicit
   o name
   (util/check-keys
    (util/hashify frames)
    (list*
     :ontology :name
     (keys owl-class-handlers)))))

(defentity defclass
  "Define a new class. Accepts a set number of frames, each marked
by a keyword :subclass, :equivalent, :annotation, :name, :comment,
:label or :disjoint. Each frame can contain an item, a list of items or any
combination of the two. The class object is stored in a var called classname."
  'tawny.owl/owl-class)

(comment
  (defmacro defclass
    "Define a new class. Accepts a set number of frames, each marked
by a keyword :subclass, :equivalent, :annotation, :name, :comment,
:label or :disjoint. Each frame can contain an item, a list of items or any
combination of the two. The class object is stored in a var called classname."
    [classname & frames]
    `(let [string-name# (name '~classname)
           class# (tawny.owl/owl-class string-name# ~@frames)]
       (intern-owl ~classname class#))))

(defdontfn disjoint-classes
  "Makes all elements in list disjoint.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre [(sequential? list)
         (> (count list) 1)]}
  (let [classlist
        (util/domap
         (fn [x]
           (ensure-class o x))
         list)]
    (add-axiom
     o
     (.getOWLDisjointClassesAxiom
      (owl-data-factory)
      (set classlist)
      (union-annotations classlist)))))

(defdontfn equivalent-classes
  "Makes all elements in list equivalent.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre [(sequential? list)
         (> (count list) 1)]}
  (let [classlist
        (doall
         (map
          (fn [x]
            (ensure-class o x))
          list))]
    (add-axiom
     o
     (.getOWLEquivalentClassesAxiom
      (owl-data-factory)
      (hset classlist)
      (union-annotations classlist)))))
;; #+end_src

;; * Individuals

;; #+begin_src clojure
(defbdontfn add-type
  {:doc "Adds CLAZZES as a type to individual to current ontology
or ONTOLOGY if present."
   :arglists '([individual & clazzes] [o individual & clazzes])}
  [o individual clazz]
  (add-axiom
   o
   (.getOWLClassAssertionAxiom
    (owl-data-factory)
    (ensure-class o clazz)
    (p/as-entity individual)
    (p/as-annotations individual))))

(defbdontfn add-fact
  {:doc "Add FACTS to an INDIVIDUAL in the current ontology or
  ONTOLOGY if present. Facts are produced with `fact' and `fact-not'."
   :arglists '([individual & facts] [ontology individual & facts])}
  [o individual fact]
  (add-axiom
   o
   ((p/as-entity fact)
    individual
    (p/as-annotations fact))))

(defmulti get-fact guess-type-args)
(defmulti get-fact-not guess-type-args)

(defmontfn fact
  "Returns a fact assertion a relation by PROPERTY which can be
either an object or data property toward either an individual or literal
TO. This is the same as the function `is'."
  [o property to]
  (fn fact [from annotations]
    (get-fact o property from to annotations)))

(def ^{:doc "Returns a fact assertion a relation by PROPERTY which can be
either an object or data property toward either an individual or literal
TO"}
  is
  fact)

(defmontfn fact-not
  "Returns a fact asserting the lack of a relationship along PROPERTY
toward an either an individual or literal TO."
  [o property to]
  (fn fact-not [from annotations]
    (get-fact-not o property from to annotations)))

(defmontfn object-get-fact
  "Returns an OWL Object property assertion axiom."
  [_ property from to annotations]
  {:pre [(say
          (t/obj-prop-exp? property)
          (t/individual? from)
          (t/individual? to))]}
  (.getOWLObjectPropertyAssertionAxiom
   (owl-data-factory)
   property from to annotations))

(defmethod get-fact ::object [& rest]
  (apply object-get-fact rest))

(defmontfn object-get-fact-not
  "Returns a negative OWL Object property assertion axiom."
  [_ property from to annotations]
  {:pre [(say
          (t/obj-prop-exp? property)
          (t/individual? from)
          (t/individual? to))]}
  (.getOWLNegativeObjectPropertyAssertionAxiom
   (owl-data-factory)
   property from to annotations))

(defmethod get-fact-not ::object [& rest]
  (apply object-get-fact-not rest))

(defdontfn
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

(defmontfn add-different
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

(defdontfn individual-explicit
  "Returns a new individual."
  [o name frames]
  (let [individual (ensure-individual o name)]
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

(defdontfn individual
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

(comment
  (defmacro defindividual
    "Declare a new individual."
    [individualname & frames]
    `(let [string-name# (name '~individualname)
           individual# (tawny.owl/individual string-name# ~@frames)]
       (intern-owl ~individualname individual#))))
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

(defdontfn as-disjoint
  {:doc "All entities are declared as disjoint. Entities may be
any structure and may also be a var. See also 'as-subclasses'."
   :arglists '([ontology & entities] [& entities])}
  [o & entities]
  (let [entities
        (map var-get-maybe (flatten entities))]
    (case
        (apply guess-type-args o
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

(defdontfn as-equivalent
  "Declare the properties or classes as disjoint."
  [o & entities]
  (let [entities
        (map var-get-maybe (flatten entities))]
    (case
        (apply guess-type-args o
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

(defdontfn as-inverse
  {:doc "Declare the two properties as inverse"
   :arglist '([ontology prop1 prop2] [prop1 prop2])}
  [o p1 p2]
  (add-inverse o
               (var-get-maybe p1)
               (var-get-maybe p2)))

(defdontfn
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
                      (owl-or o subclasses)))))

(defdontfn
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
(defdontfn direct-superclasses
  "Returns the direct superclasses of name.
Name can be either a class or a string name. Returns a list of class
expressions."
  [^OWLOntology o name]
  (let [^OWLClass clz (ensure-class o name)]
    ;; general Class expressions return empty
    (if (t/owl-class? clz)
      (EntitySearcher/getSuperClasses clz o)
      ())))

(defdontfn superclasses
  "Return all superclasses of class.
class is not returned unless it is explicitly stated to be a
direct or indirect superclass of itself."
  [o class]
  (recurse-ontology-tree
   o direct-superclasses class))

(defdontfn superclass?
  "Returns true if name is asserted to be a superclass."
  [o name superclass]
  (let [namecls (ensure-class o name)
        superclasscls (ensure-class o superclass)]
    (some #(.equals superclasscls %) (superclasses o name))))

(defdontfn direct-subclasses
  "Returns the direct subclasses of name."
  [^OWLOntology o name]
  (let [clz (ensure-class o name)]
    (if (t/owl-class? clz)
      (EntitySearcher/getSubClasses clz o) ())))

(defdontfn subclasses
  "Return all subclasses of class."
  [o class]
  (recurse-ontology-tree
   o direct-subclasses class))

(defdontfn subclass?
  "Returns true if name has subclass as a subclass."
  [o name subclass]
  (let [namecls (ensure-class o name)
        subclasscls (ensure-class o subclass)]
    (some #(.equals subclasscls %) (subclasses o name))))

(defdontfn disjoint?
  "Returns t iff entities (classes or properties) are asserted to be
  disjoint."
  [^OWLOntology o a b]
  (let [type (guess-type-args o a b)]
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

(defdontfn equivalent?
  "Returns t iff classes are asserted to be equivalent."
  [^OWLOntology o a b]
  (let [type (guess-type-args o a b)]
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

(defdontfn inverse?
  "Returns t iff properties are asserted to be inverse"
  [^OWLOntology o ^OWLObjectProperty p1 ^OWLObjectProperty p2]
  (contains?
   (EntitySearcher/getInverses p1 o) p2))

(defdontfn direct-superproperties
  "Return all direct superproperties of property."
  [^OWLOntology o property]
  (EntitySearcher/getSuperProperties
   (ensure-property o property) o))

(defdontfn superproperties
  "Return all superproperties of a property."
  [o property]
  (recurse-ontology-tree
   o direct-superproperties
   (ensure-property o property)))

(defdontfn superproperty?
  "Return true if superproperty is a superproperty of property."
  [o property superproperty]
  (some #(.equals
          (ensure-property o superproperty) %)
        (superproperties o property)))

(defdontfn direct-subproperties
  "Returns all direct subproperties of property."
  [^OWLOntology o
   property]
  (EntitySearcher/getSubProperties
   (ensure-property o property) o))

(defdontfn subproperties
  "Returns all subproperties of property."
  [o property]
  (recurse-ontology-tree
   o direct-subproperties
   (ensure-property o property)))

(defdontfn subproperty?
  "Returns true if property is a subproperty of subproperty."
  [o property subproperty]
  (some #(.equals
          (ensure-property o subproperty) %)
        (subproperties o property)))

(defdontfn direct-instances
  "Return all direct instances of NAME class."
  [^OWLOntology o name]
  (let [clz (ensure-class o name)]
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

(defmontfn
  ^:private
  entity-class [o entity & _]
  (class entity))

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

(defmethod refine OWLClass [& args]
  (apply owl-class args))

(defmethod refine OWLObjectProperty [& args]
  (apply object-property args))

(defmethod refine OWLAnnotationProperty [& args]
  (apply annotation-property args))

(defmethod refine OWLDataProperty [& args]
  (apply datatype-property args))

(defmethod refine OWLDatatype [& args]
  (apply datatype args))

(defmethod refine OWLIndividual [& args]
  (apply individual args))


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
