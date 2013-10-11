;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2012, 2013, Newcastle University

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

(ns ^{:doc "Build ontologies in OWL."
      :author "Phillip Lord"}
  tawny.owl
  (:require
   [clojure.walk :only postwalk]
   [clojure.set]
   [tawny.util :as util])
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation
                                 OWLIndividual OWLDatatype
                                 OWLNamedObject OWLOntologyID
                                 OWLAnnotationProperty OWLObjectProperty
                                 OWLDataProperty OWLDataRange
                                 OWLDataPropertyExpression OWLLiteral)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.coode.owlapi.manchesterowlsyntax ManchesterOWLSyntaxOntologyFormat)
   (org.semanticweb.owlapi.io StreamDocumentTarget OWLXMLOntologyFormat
                              RDFXMLOntologyFormat)
   (org.semanticweb.owlapi.util DefaultPrefixManager OWLEntityRemover)
   (java.io ByteArrayOutputStream FileOutputStream PrintWriter)
   (java.io File)
   (java.util Collections)
   (org.semanticweb.owlapi.model AddAxiom RemoveAxiom AddImport
                                 AddOntologyAnnotation)))

;;; Begin resource section

;; The next three forms all contain values that percolate across all the
;; namespaces that contain ontologies. Resetting all of these when owl.clj is
;; eval'd is a pain, hence they are all defonce.
(defonce
  ^{:doc "A java object which is the main factory for all other objects"
    }
  ontology-data-factory
  (OWLManager/getOWLDataFactory))

(defonce
  ^{:doc "The single OWLOntologyManager used by tawny."}
  owl-ontology-manager
  (OWLManager/createOWLOntologyManager ontology-data-factory))

(defonce
  ^{:doc "Map between namespaces and ontologies"}
  ontology-for-namespace (ref {}))

(defn iri
  "Returns an IRI object given a string. Does no transformation on the
string; use 'iri-for-name' to perform ontology specific expansion"
  [name]
  (IRI/create name))

(load "owl_self")

(defn named-object?
  "Returns true iff entity is an OWLNamedObject."
  [entity]
  (instance? OWLNamedObject entity))

(defn as-named-object
  "If entity is a named object do nothing, else throw
an exception."
  [entity]
  (or
   (and (instance? OWLNamedObject entity)
        entity)
   (throw (IllegalArgumentException. "Expecting a named entity"))))

;;; Begin current ontology support

;; not sure this is necessary now as (almost) all functions now take an
;; ontology parameter, which is generally going to be the nicer way to achieve
;; things.
(def
  ^{:dynamic true
    :doc "The currently bound ontology. If this is not set, then the current
ontology is expected to be bound to the current namespace with 'defontology'
or similar macros. Normally, when set, this is set with the 'with-ontology'
macro." }
  *current-bound-ontology* nil)

(defmacro with-ontology
  "Sets the default ontology for all operations inside its dynamic scope.

This may be deprecated in future. Where relevant, functions now accept an
ontology as a first argument."
  [o & body]
  `(binding [tawny.owl/*current-bound-ontology* ~o]
     ~@body))

(defn get-current-ontology-maybe
  "Gets the current ontology, or nil if there is not one."
  ([]
     (get-current-ontology-maybe *ns*))
  ([ns]
     ;; if current ontology is inside a binding
     (or *current-bound-ontology*
         ;; so use the namespace bound one
         (get @ontology-for-namespace ns))))

(defn get-current-ontology
  "Gets the current ontology. Throws an exception if there is not current
ontology"
  ([]
     (get-current-ontology *ns*))
  ([ns]
     (or (get-current-ontology-maybe ns)
         ;; so break
         (throw (IllegalStateException. "Current ontology has not been set")))))

(defmacro defnwithfn
  "Define a new var like defn, but compose FUNCTION with BODY before
rather than just using BODY directly."
  [name function & body]
  `(let [vr# (defn ~name ~@body)]
     (alter-var-root
      vr#
      (fn [f#]
        (fn ~name [& args#]
          (apply ~function f# args#))))
     vr#))

(defonce
  ^{:doc "Hook called when the default ontology is used"}
  default-ontology-hook (util/make-hook))

(defn default-ontology-maybe
  "Invoke f ensuring the first argument is an ontology or nil.
The logic used is the same as default-ontology except that no error is
signalled if there is no current-ontology."
  [f & args]
  (let [pre-ontology
        (drop-while #(not (= :ontology %)) args)
        ontology (second pre-ontology)]
    (if (or
         (instance?
          org.semanticweb.owlapi.model.OWLOntology (first args))
         (nil? (first args)))
      (apply f args)
      (if ontology
        (apply f ontology args)
        (do
          ;; a call back so we can block this if we choose
          (util/run-hook default-ontology-hook)
          (apply f (get-current-ontology-maybe) args))))))

(defn default-ontology
  "Invoke f ensuring that the first argument is an OWLOntology.
If the first element of args is an OWLOntology, this just means apply the
ontology. If not, and args contains a :ontology o subsequence, invoke (f o
args). If neither mechanism specifies an ontology, call get-current-ontology.
If there is no current ontology, then an error will be thrown."
  [f & args]
  (let [pre-ontology
        (drop-while #(not (= :ontology %)) args)
        ontology (second pre-ontology)]
    (if
        (instance?
         org.semanticweb.owlapi.model.OWLOntology (first args))
      (apply f args)
      (if ontology
        (apply f ontology args)
        (do
          ;; a call back so we can block this if we choose
          (util/run-hook default-ontology-hook)
          (apply f (get-current-ontology) args))))))

(defmacro defdontfn
  "Like defn, but automatically adds the current-ontology to the args
if the first arg is not an ontology. Throws an IllegalStateException
if there is no current ontology.

The 'd' stands for definately."
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

(defn broadcast-ontology-maybe
  "Like broadcast-ontology but does not signal an error if there is no current
ontology."
  [f & args]
  (apply default-ontology-maybe
         (fn broadcast-ontology-maybe [o & narg]
           (doall
            (map (partial f o (first narg))
                 (filter identity
                         (flatten
                          (rest narg))))))
         args))

(defn broadcast-ontology
  "Given a function which expects an ontology and two other arguments, ensure
that the first argument is an ontology (see default-ontology for details),
then f repeatedly against the second args and all flatten subsequent
arguments."
  [f & args]
  (apply default-ontology
         (fn [o & narg]
           (doall
            (map (partial f o (first narg))
                 (filter identity
                         (flatten
                          (rest narg))))))
         args))

(defmacro defbdontfn
  "Like the defn and defdontfn, but broadcasts. That is it expects a three arg
function, f with ontology, x and y, but defines a new function which takes
ontology, x and rest, returning a list which is f applied to ontology, x and
the flatttened elements of rest.

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

;;; Axiom mainpulation
(defdontfn add-axiom
  "Adds an axiom from the given ontology, or the current one."
  [o axiom]
  (.applyChange owl-ontology-manager
                (AddAxiom. o axiom))
  axiom)

(defdontfn remove-axiom
  "Removes a list of axioms from the given ontology, or the current one."
  [o & axiom-list]
  (doall
   (map (fn [axiom]
          (do
            (.applyChange owl-ontology-manager
                          (RemoveAxiom. o axiom))))
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
  [o entity]
  (let [remover
        (OWLEntityRemover. owl-ontology-manager
                           (hash-set o))]
    (.accept entity remover)
    (.applyChanges owl-ontology-manager
                   (.getChanges remover))))

;;; Begin Ontology options

;; ontology options -- additional knowledge that I want to attach to each
;; ontology,  but which gets junked when the ontology does.
(def ^{:doc "Ontology options. A map on a ref for each ontology"}
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

;;; Begin iri support
(defdontfn get-iri
  "Gets the IRI for the given ontology, or the current ontology if none is given"
  [o]
  (.getOntologyIRI
   (.getOntologyID o)))

(defdontfn iri-for-name
  "Returns an IRI object for the given name.

This is likely to become a property of the ontology at a later date, but at
the moment it is very simple."
  [o name]
  (if-let [iri-gen (:iri-gen (deref (ontology-options o)))]
    (iri-gen name)
    (iri (str (get-iri o) "#" name))))

;;; Begin interned-entity-support
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

;; In the idea world, these would be one function. However, they can't be. The
;; intern version works where we do not know the symbol at compile time. This
;; is generally useful for read'ing and the like. The symbol created cannot be
;; used in the same form because the compiler doesn't know that it has been
;; defined.
(defn intern-owl-string
  "Interns an OWL Entity. Compared to the clojure.core/intern function this
signals a hook, and adds :owl true to the metadata. NAME must be a strings"
  ([name entity]
     (tawny.owl/run-intern-hook
      (intern *ns*
              (with-meta
                (symbol name)
                {:owl true})
              entity))))

;; While this version uses def semantics -- the point here is that the def
;; form is expanded at compile time, the compiler recognises that the form has
;; been defined, and so allows subsequent referencing of the symbol within the
;; same form.
(defmacro intern-owl
  "Intern an OWL Entity. Compared to the clojure.core/intern function this
signals a hook, and adds :owl true to the metadata. NAME must be a symbol"
  ([name entity]
     ;; we use def semantics here, rather than intern-owl-string, because
     ;; intern is not picked up by the compiler as defining a symbol
     `(tawny.owl/run-intern-hook
       (def ~(vary-meta name
                        merge
                        {:owl true})
         ~entity))))

;;; Begin OWL2 datatypes
(def
  ^{:doc "A map of keywords to the OWL2Datatypes values"}
  owl2datatypes
  (into {}
        (for [k (org.semanticweb.owlapi.vocab.OWL2Datatype/values)]
          [(keyword (.name k)) k])))

;;; Begin Annotation Support

;; annotations
(defbdontfn add-a-simple-annotation
  "Adds an annotation to a named object."
  [o named-entity annotation]
  (let [axiom
        (.getOWLAnnotationAssertionAxiom
         ontology-data-factory
         (.getIRI named-entity) annotation)]
    (add-axiom o axiom)))

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

(defbdontfn add-an-ontology-annotation
  "Adds an annotation to an ontology."
  [o o annotation]
  (.applyChange
   owl-ontology-manager
   (AddOntologyAnnotation. o annotation)))

(defdontfn add-annotation
  {:doc "Add an annotation in the ontology to either the named-entity
or the ontology. Broadcasts over annotations."
   :arglists '([ontology named-entity & annotations]
                 [named-entity & annotations]
                   [ontology & annotations][annotations])}
  [o & args]
  (if (named-object? (first args))
    (add-a-simple-annotation
     o (first args) (rest args))
    (add-an-ontology-annotation
     ;; okay, so this is wierd, but broadcasting requires a three arg
     ;; function, first being an ontology.
     o o args)))

(defn- ensure-annotation-property
  "Ensures that 'property' is an annotation property,
converting it from a string or IRI if necessary."
  [o property]
  (cond
   (instance? OWLAnnotationProperty property)
   property
   (instance? IRI property)
   (.getOWLAnnotationProperty
    ontology-data-factory property)
   (instance? String property)
   (ensure-annotation-property
    o
    (iri-for-name o property))
   :default
   (throw (IllegalArgumentException.
           (format "Expecting an OWL annotation property: %s" property)))))

(defmontfn annotation
  "Creates a new annotation property. If literal is a string it is
interpreted as a String in English. Alternatively, a literal created
with the literal function."
  ([o annotation-property literal]
     (cond
      (instance? String literal)
      (annotation o annotation-property literal "en")
      (instance? OWLLiteral literal)
      (.getOWLAnnotation
       ontology-data-factory
       (ensure-annotation-property o annotation-property)
       literal)
      :default
      (throw (IllegalArgumentException.
              "annotation takes a String or OWLLiteral"))))
  ([o annotation-property literal language]
     (annotation o annotation-property
                 (.getOWLLiteral ontology-data-factory literal language))))

(defbdontfn add-super-annotation
  "Adds a set of superproperties to the given subproperty."
  [o subproperty superproperty]
  (.applyChange owl-ontology-manager
                (AddAxiom.
                 o
                 (.getOWLSubAnnotationPropertyOfAxiom
                  ontology-data-factory
                  subproperty
                  (ensure-annotation-property o superproperty)))))


;; various annotation types
(def label-property
  (.getRDFSLabel ontology-data-factory))

(defmontfn label
  "Return an OWL label annotation."
  [o & args]
  (apply annotation o label-property args))

(def owl-comment-property
  (.getRDFSComment ontology-data-factory))

(defmontfn owl-comment
  "Return an OWL comment annotation."
  [o & args]
  (apply annotation o owl-comment-property args))

(def is-defined-by-property
  (.getRDFSIsDefinedBy ontology-data-factory))

(defmontfn is-defined-by
  "Return an is defined by annotation."
  [o & args]
  (apply annotation o is-defined-by-property args))

(def see-also-property
  (.getRDFSSeeAlso ontology-data-factory))

(defmontfn see-also
  "Returns a see-also annotation."
  [o & args]
  (apply annotation o
         see-also-property args))

(def backward-compatible-with-property
  (.getOWLBackwardCompatibleWith ontology-data-factory))

(defmontfn backward-compatible-with
  "Returns a backward compatible with annotation."
  [o & args]
  (apply annotation o
         backward-compatible-with-property
         args))

(def incompatible-with-property
  (.getOWLIncompatibleWith ontology-data-factory))

(defmontfn incompatible-with
  "Returns an incompatible with annotation."
  [o & args]
  (apply annotation o
         incompatible-with-property
         args))

(def version-info-property
  (.getOWLVersionInfo ontology-data-factory))

(defmontfn version-info
  "Returns a version info annotation."
  [o & args]
  (apply annotation o
         version-info-property
         args))

(def deprecated-property
  (.getOWLDeprecated ontology-data-factory))

(defmontfn deprecated
  "Returns a deprecated annotation."
  [o & args]
  (apply annotation o
         deprecated-property
         args))

(defbmontfn add-label
  "Add labels to the named entities."
  [o named-entity label]
  (add-annotation
   o
   named-entity
   [(tawny.owl/label label)]))

(defbmontfn add-comment
  "Add comments to the named entities."
  [o named-entity comment]
  (add-annotation o named-entity
                  [(owl-comment comment)]))


(def ^{:private true} annotation-property-handlers
  {
   :subproperty add-super-annotation
   :annotation add-annotation
   :comment add-comment
   :label add-label
   })

(defdontfn annotation-property-explicit
  "Add this annotation property to the ontology"
  [o name frames]
  (let [o
        (or (first (get frames :ontology)) o)
        property
        (ensure-annotation-property o name)
        ]
    ;; add the property
    (.addAxiom owl-ontology-manager
               o
               (.getOWLDeclarationAxiom
                ontology-data-factory
                property))
    ;; add a name annotation
    (add-a-name-annotation o property name)
    ;; apply the handlers
    (doseq [[k f] annotation-property-handlers]
      (f o property (get frames k)))
    ;; return the property
    property))

(defdontfn annotation-property
  {:doc "Creates a new annotation property."
   :arglists '([ontology name & frames] [name & frames])}
  [o name & frames]
  (annotation-property-explicit o
                                name
                                (util/check-keys
                                 (util/hashify frames)
                                 (list* :ontology
                                        (keys annotation-property-handlers)))))

(defn- get-annotation-property
  "Gets an annotation property with the given name."
  [o property]
  (.getOWLAnnotationProperty
   ontology-data-factory
   (iri-for-name o property)))

(defmacro defaproperty
  "Defines a new annotation property in the current ontology.
See 'defclass' for more details on the syntax."
  [property & frames]
  `(let [property-name# (name '~property)
         property#
         (tawny.owl/annotation-property property-name# ~@frames)]
     (intern-owl ~property property#)))

;;; Ontology manipulation

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

(def
  ^{:doc "Hook called immediately after an ontology is removed from the
owl-ontology-manager."}
  remove-ontology-hook (util/make-hook))

(defn remove-ontology-maybe
  "Removes the ontology with the given ID from the manager.
This calls the relevant hooks, so is better than direct use of the OWL API. "
  [ontologyid]
  (when (.contains owl-ontology-manager ontologyid)
    (let [o (.getOntology owl-ontology-manager ontologyid)]
      (.removeOntology
       owl-ontology-manager o)
      ;; remove the ontology options
      (dosync
       (swap! ontology-options-atom
              dissoc o))
      ;; remove the ontology from the namespace map
      (remove-ontology-from-namespace-map o)
      (util/run-hook remove-ontology-hook o))))

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

(defn- set-iri-gen
  "Add an IRI gen function to the ontology options."
  [o f]
  (if f
    (dosync
     (alter (ontology-options o)
            merge {:iri-gen f}))))

(defn- set-prefix
  "Sets a prefix for the ontology."
  [o p]
  (if p
    (.setPrefix
     (.getOntologyFormat
      owl-ontology-manager o)
     p (.toString (get-iri o)))))

(defn- add-see-also
  "Adds a see also annotation to the ontology"
  [o s]
  (if s
    (add-annotation o (see-also o s))))

(defn- add-version-info
  "Adds a version info annotation to the ontology."
  [o v]
  (if v
    (add-version-info o v)))

;; owl imports
(defn owl-import
  "Adds a new import to the current ontology. o may be an
ontology or an IRI"
  ([o]
     (owl-import (get-current-ontology) o))
  ([ontology-into o]
     (let [iri (if (instance? OWLOntology o)
                 (get-iri o)
                 o)]
       (.applyChange owl-ontology-manager
                     (AddImport. ontology-into
                                 (.getOWLImportsDeclaration
                                  ontology-data-factory
                                  iri))))))
;; ontology
(def ^{:private true} ontology-handlers
  {:iri-gen set-iri-gen,
   :prefix set-prefix,
   :name add-an-ontology-name
   :seealso add-see-also
   :comment add-comment
   :versioninfo add-version-info
   })

(defn ontology
  "Returns a new ontology. See 'defontology' for full description."
  [& args]
  (let [options (apply hash-map args)
        ;; the prefix is specified by the prefix or the name.
        ;; this allows me to do "(defontology tmp)"
        options (merge options
                       {:prefix (or (:prefix options)
                                    (:name options))})
        iri (IRI/create (get options :iri
                             (str
                              (.toString (java.util.UUID/randomUUID))
                              (if-let [name
                                       (get options :name)]
                                (str "#" name)))))
        noname (get options :noname false)
        ]
    (remove-ontology-maybe
     (OWLOntologyID. iri))
    (let [ontology
          (.createOntology owl-ontology-manager iri)]
      (if noname
        (dosync
         (alter
          (tawny.owl/ontology-options ontology)
          merge {:noname true}))
        (owl-import ontology tawny-iri))
      (doseq [[k f] ontology-handlers]
        (f ontology (get options k)))
      ontology)))

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
           (def
             ~(with-meta name
                (assoc (meta name)
                  :owl true))
             ontology#)]
       (tawny.owl/ontology-to-namespace ontology#)
       var#
       )))

(defn test-ontology
  "Define a test ontology.

This function probably shouldn't be here, but one of the consequences of
making the ontology implicit in all my functions is that playing on the repl
is a pain, as the test ontology has to be defined first.

This defines a minimal test ontology.

"
  ([]
     (test-ontology *ns*))
  ([ns]
     (let [o (ontology :iri "http://iri/" :prefix "test:")]
       (ontology-to-namespace o)
       (intern ns 'a-test-ontology o))))

;;; Begin ontology look up functions
(defn- check-entity-set
  [entity-set iri]
  ;; ontology could be in full, or could be punning. Either way we are
  ;; stuffed.
  (when (< 1 (count entity-set))
    (throw
     (IllegalArgumentException.
      (format "Can not uniquely determine type of IRI: %s,%s" iri entity-set))))
  ;; IRI appears once; happiness
  (if (= 1 (count entity-set))
    (first entity-set)
    ;; IRI appears not at all
    nil))

(defdontfn entity-for-iri
  "Return the OWLObject for a given IRI if it exists, checking
'ontology' first, but checking all loaded ontologies.

This function uses a heuristic to find the right entity. If you want
more control use 'check-entity-set' and the '.getEntitiesInSignature'
method of OWLOntology."
  [o iri]
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
     (map #(.getEntitiesInSignature % iri true)
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

(defdontfn get-prefix
  "Returns the prefix for the given ontology, or the current ontology if none
is given."
  [o]
  ;; my assumption here is that there will only ever be one prefix for a given
  ;; ontology. If not, it's all going to go wrong.
  (first
   (keys
    (.getPrefixName2PrefixMap
     (.getOntologyFormat owl-ontology-manager
                         o)))))

(defdontfn save-ontology
  "Save the current 'ontology' in the file or `filename' if given.
If no ontology is given, use the current-ontology"
  ([o filename]
     (save-ontology o filename (ManchesterOWLSyntaxOntologyFormat.)
                    (str "## This file was created by Tawny-OWL\n"
                         "## It should not be edited by hand\n" )))
  ([o filename format]
     (save-ontology o filename format ""))
  ([o filename format prepend]
     (let [file (new File filename)
           output-stream (new FileOutputStream file)
           file-writer (new PrintWriter output-stream)
           this-format
           (cond
            (= format :rdf) (RDFXMLOntologyFormat.)
            (= format :omn) (ManchesterOWLSyntaxOntologyFormat.)
            (= format :owl) (OWLXMLOntologyFormat.)
            :else format)]
       (when (.isPrefixOWLOntologyFormat this-format)
         (dorun
          (map #(.setPrefix this-format (get-prefix %)
                            (str (.toString (get-iri %)) "#"))
               (vals @ontology-for-namespace))))
       (.print file-writer prepend)
       (.flush file-writer)
       (.setPrefix this-format (get-prefix o)
                   (str (.toString (get-iri o)) "#"))
       (.saveOntology owl-ontology-manager o
                      this-format output-stream))))

;;; Begin OWL entity guess/ensure
(defmontfn guess-type
  "Guesses the type of the entity. Returns :object, :data or :annotation or
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
  (let [oneof? (partial some (fn[n] (instance? n entity)))]
    (cond
     ;; it's a collection -- find the first entity
     (coll? entity)
     (some (partial guess-type o) entity)
     ;; return if individual, class, datatype
     (oneof?
      [OWLClassExpression OWLObjectProperty])
     :object
     (oneof?
      [OWLAnnotationProperty])
     :annotation
     (oneof?
      [OWLDataRange OWLDataPropertyExpression])
     :data
     ;; up to this point, o can be nil -- after this point, we need to know
     ;; the ontology we are searching in.
     ;; if an IRI, see if it is the current ontology
     (instance? IRI entity)
     (guess-type o (entity-for-iri o entity))
     ;; keyword -- these are builtin OWL2Datatypes
     (and (keyword? entity)
          (get owl2datatypes entity))
     :data
     ;; string name in current ontology?
     (string? entity)
     (guess-type o (entity-for-string o entity))
     ;; owl individuals tell us nothing, cause we still don't know!
     (instance? OWLIndividual entity)
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
  (cond
   (coll? entity)
   (some (partial guess-individual-literal o) entity)
   (instance? OWLIndividual entity)
   :individual
   (instance? OWLLiteral entity)
   :literal
   (instance? IRI entity)
   (guess-individual-literal o
                             (entity-for-iri o entity))
   (string? entity)
   (guess-individual-literal o
                             (entity-for-string o entity))
   :default
   (throw (IllegalArgumentException.
           (str "Cannot tell if this is individual or literal:" entity)))))

(defn-
  ^{:private true}
  ensure-object-property
  "Ensures that the entity in question is an OWLObjectProperty
or throw an exception if it cannot be converted."
  [o prop]
  (cond
   (fn? prop)
   (ensure-object-property o (prop))
   (instance? OWLObjectProperty prop)
   prop
   (instance? IRI prop)
   (.getOWLObjectProperty ontology-data-factory prop)
   (string? prop)
   (ensure-object-property o (iri-for-name o prop))
   :default
   (throw (IllegalArgumentException.
           (str "Expecting an object property. Got: " prop)))))

(defn- ensure-class
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  [o clz]
  (cond
   (fn? clz)
   (ensure-class o (clz))
   (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
   clz
   (instance? IRI clz)
   (.getOWLClass ontology-data-factory clz)
   (string? clz)
   (ensure-class o (iri-for-name o clz))
   true
   (throw (IllegalArgumentException.
           (str "Expecting a class. Got: " clz)))))

(defn- ensure-data-property [o property]
  "Ensures that 'property' is an data property,
converting it from a string or IRI if necessary."
  (cond
   (instance? OWLDataProperty property)
   property
   (instance? IRI property)
   (.getOWLDataProperty
    ontology-data-factory property)
   (instance? String property)
   (ensure-data-property o
                         (iri-for-name o property))
   :default
   (throw (IllegalArgumentException.
           (format "Expecting an OWL data property: %s" property)))))

(defn- ensure-datatype
  "Ensure that 'datatype' is an OWLDatatype. Will convert from an keyword for
  builtin datatypes."
  [o datatype]
  (cond
   (instance? OWLDatatype datatype)
   datatype
   (instance? org.semanticweb.owlapi.vocab.OWL2Datatype datatype)
   (.getDatatype datatype ontology-data-factory)
   (keyword? datatype)
   (if-let [d (get owl2datatypes datatype)]
     (ensure-datatype o d)
     (throw (IllegalArgumentException.
             (str "Was expecting a datatype. Got " datatype "."))))
   (instance? IRI datatype)
   (.getOWLDatatype datatype ontology-data-factory)
   :default
   (throw (IllegalArgumentException.
           (str "Was expecting a datatype. Got " datatype ".")))))

(defn- ensure-data-range
  "Ensure that 'data-range' is a OWLDataRange either directly or
as a datatype."
  [o data-range]
  (cond
   (instance? org.semanticweb.owlapi.model.OWLDataRange data-range)
   data-range
   :default
   (ensure-datatype o data-range)))

(defn- ensure-individual
  "Returns an INDIVIDUAL.
If INDIVIDUAL is an OWLIndividual return individual, else
interpret this as a string and create a new OWLIndividual."
  [o individual]
  (cond (instance? org.semanticweb.owlapi.model.OWLIndividual individual)
        individual
        (instance? IRI individual)
        (.getOWLNamedIndividual ontology-data-factory
                                individual)
        (string? individual)
        (ensure-individual o (iri-for-name o individual))
        :default
        (throw (IllegalArgumentException.
                (str "Expecting an Individual. Got: " individual)))))


;; Begin add-owl objects
(defbdontfn
  add-subclass
  {:doc "Adds one or more subclass to name in ontology."
   :arglists '([name & subclass] [ontology name & subclass])}
  [o name subclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              ontology-data-factory
              (ensure-class o name)
              (ensure-class o subclass))))

(defbdontfn add-equivalent
  {:doc "Adds an equivalent axiom to the ontology."
   :arglists '([name equivalent] [ontology name equivalent])
   }
  [o name equivalent]
  (add-axiom o
             (.getOWLEquivalentClassesAxiom
              ontology-data-factory
              (ensure-class o name)
              (ensure-class o equivalent))))

(defbdontfn add-disjoint
  {:doc "Adds a disjoint axiom to the ontology."
   :arglists '([name disjoint] [ontology name disjoint])}
  [o name disjoint]
  (add-axiom
   o
   (.getOWLDisjointClassesAxiom
    ontology-data-factory
    (into-array OWLClassExpression
                [(ensure-class o name)
                 (ensure-class o disjoint)]))))

(defdontfn add-disjoint-union
  "Adds a disjoint union axiom to all subclasses."
  [o clazz subclasses]
  (let [ensured-subclasses
        (util/domap #(ensure-class o %) subclasses)
        ]
    (list
     (add-axiom o
                (.getOWLDisjointUnionAxiom
                 ontology-data-factory
                 (ensure-class o clazz)
                 (java.util.HashSet. ensured-subclasses))))))

(defdontfn add-class
  "Adds a class to the ontology."
  [o name]
  (add-axiom o
             (.getOWLDeclarationAxiom
              ontology-data-factory
              (ensure-class o name))))

;; a class can have only a single haskey, so ain't no point broadcasting this.
(defdontfn add-has-key
  "Adds a has-key to the class."
  [o class propertylist]
  ;; nil or empty list safe
  (if (seq propertylist)
    (let [type (guess-type o propertylist)
          propertylist
          (cond
           (= :object type)
           (map (partial ensure-object-property o) propertylist)
           (= :data type)
           (map (partial ensure-data-property o) propertylist)
           :default
           (throw
            (IllegalArgumentException.
             "Unable to determine type of property")))]
      (add-axiom o
                 (.getOWLHasKeyAxiom ontology-data-factory
                                     (ensure-class o class)
                                     (into #{} propertylist))))))

(defbdontfn add-domain
  "Adds all the entities in domainlist as domains to a property."
  [o property domain]
  (add-axiom o
             (.getOWLObjectPropertyDomainAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-class o domain))))

(defbdontfn add-range
  "Adds all the entities in rangelist as range to a property."
  [o property range]
  (add-axiom o
             (.getOWLObjectPropertyRangeAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-class o range))))

(defbdontfn add-inverse
  "Adds all the entities in inverselist as inverses to a property."
  [o property inverse]
  (add-axiom o
             (.getOWLInverseObjectPropertiesAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-object-property o inverse))))


(defbdontfn add-superproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property superproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-object-property o superproperty))))

;; broadcasts specially
(defdontfn add-subpropertychain
  "Adds a property chain to property."
  [o property superpropertylist]
  (when superpropertylist
    (let [property (ensure-object-property o property)
          lists (filter sequential? superpropertylist)
          properties (filter (comp not sequential?) superpropertylist)
          ]
      (list
       ;; add individual entities are a single chain
       (add-axiom o
                  (.getOWLSubPropertyChainOfAxiom
                   ontology-data-factory
                   (map (partial ensure-object-property o) properties)
                   property))
       ;; add sequential entities as a chain in their own right
       (map (partial add-subpropertychain
                     o property)
            lists)))))


(def
  ^{:private true}
  charfuncs
  {:transitive #(.getOWLTransitiveObjectPropertyAxiom %1 %2)
   :functional #(.getOWLFunctionalObjectPropertyAxiom %1 %2)
   :inversefunctional #(.getOWLInverseFunctionalObjectPropertyAxiom %1 %2)
   :symmetric #(.getOWLSymmetricObjectPropertyAxiom %1 %2)
   :asymmetric #(.getOWLAsymmetricObjectPropertyAxiom %1 %2)
   :irreflexive #(.getOWLIrreflexiveObjectPropertyAxiom %1 %2)
   :reflexive #(.getOWLReflexiveObjectPropertyAxiom %1 %2)
   })

(defbdontfn add-characteristics
  "Add a list of characteristics to the property."
  [o property characteristic]
  (when-not (get charfuncs characteristic)
    (throw (IllegalArgumentException.
            "Characteristic is not recognised:" characteristic)))
  (add-axiom o
             ((get charfuncs characteristic)
              ontology-data-factory (ensure-object-property o property))))

(def ^{:private true} object-property-handlers
  {
   :domain add-domain
   :range add-range
   :inverse add-inverse
   :subproperty add-superproperty
   :characteristic add-characteristics
   :subpropertychain add-subpropertychain
   :annotation add-annotation
   :label add-label
   :comment add-comment})

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
                  ontology-data-factory property))
      ;; add a name annotation
      (add-a-name-annotation o property name)
      ;; apply the handlers
      (doseq [[k f] object-property-handlers]
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

(defmacro defoproperty
  "Defines a new object property in the current ontology."
  [property & frames]
  `(let [property-name# (name '~property)
         property# (tawny.owl/object-property property-name# ~@frames)]
     (intern-owl ~property property#)))

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

;; multi methods for overloaded entities. We guess the type of the arguments,
;; which can be (unambiguous) OWLObjects, potentially ambiguous IRIs or
;; strings. If we really can tell, we guess at objects because I like objects
;; better.
(defmulti owl-some #'guess-type-args)
(defmulti only #'guess-type-args)
(defmulti some-only #'guess-type-args)
(defmulti owl-and #'guess-type-args)
(defmulti owl-or #'guess-type-args)
(defmulti owl-not #'guess-type-args)
(defmulti exactly #'guess-type-args)
(defmulti oneof #'guess-individual-literal-args)
(defmulti at-least #'guess-type-args)
(defmulti at-most #'guess-type-args)
(defmulti has-value #'guess-type-args)

(defn guess-type-error
  "Throws an exception always"
  [& args]
  (throw (IllegalArgumentException.
          (str "Unable to determine the type of: " args))))

;; use .addMethod directly so we can use a single function repeatedly.
(.addMethod owl-some nil guess-type-error)
(.addMethod only nil guess-type-error)
(.addMethod some-only nil guess-type-error)
(.addMethod owl-and nil guess-type-error)
(.addMethod owl-or nil guess-type-error)
(.addMethod owl-not nil guess-type-error)
(.addMethod exactly nil guess-type-error)
(.addMethod at-least nil guess-type-error)
(.addMethod at-most nil guess-type-error)
(.addMethod has-value nil guess-type-error)

;; short cuts for the terminally lazy. Still prefix!
(def && owl-and)
(def || owl-or)
(def ! owl-not)

;; "long cuts" for consistency with some
(def owl-only only)

(defbmontfn object-some
  {:doc "Returns an OWL some values from restriction."
   :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectSomeValuesFrom
   ontology-data-factory
   (ensure-object-property o property)
   (ensure-class o class)))

;; use add method because we want object-some to have independent life!
(.addMethod owl-some :object object-some)

(defbmontfn object-only
  {:doc "Returns an OWL all values from restriction."
   :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectAllValuesFrom
   ontology-data-factory
   (ensure-object-property o property)
   (ensure-class o class)))

(.addMethod only :object object-only)

;; union, intersection
(defmontfn object-and
  "Returns an OWL intersection of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-and must have at least two classes")))

    (.getOWLObjectIntersectionOf
     ontology-data-factory
     (java.util.HashSet.
      (util/domap
       #(ensure-class o %)
       ;; flatten list for things like owl-some which return lists
       classes)))))

;; add to multi method
(.addMethod owl-and :object object-and)

(defmontfn object-or
  "Returns an OWL union of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owl-or must have at least two classes")))

    (.getOWLObjectUnionOf
     ontology-data-factory
     (java.util.HashSet.
      (util/domap #(ensure-class o %)
                  (flatten classes))))))

(.addMethod owl-or :object object-or)

;; lots of restrictions return a list which can be of size one. so all these
;; functions take a list but ensure that it is of size one.
(defmontfn object-not
  "Returns an OWL complement of restriction."
  [o & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectComplementOf
   ontology-data-factory
   (ensure-class o (first (flatten class)))))

(.addMethod owl-not :object object-not)

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

(.addMethod some-only :object object-some-only)

;; cardinality
(defmontfn object-at-least
  "Returns an OWL at-least cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectMinCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod at-least :object object-at-least)

(defmontfn object-at-most
  "Returns an OWL at-most cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectMaxCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod at-most :object object-at-most)

(defmontfn object-exactly
  "Returns an OWL exact cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectExactCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod exactly :object object-exactly)

(defmontfn object-oneof
  "Returns an OWL one of property restriction."
  [o & individuals]
  (.getOWLObjectOneOf
   ontology-data-factory
   (java.util.HashSet.
    (doall
     (map (partial ensure-individual o)
          (flatten individuals))))))

(.addMethod oneof :individual object-oneof)

(defmontfn object-has-value
  "Adds an OWL has-value restriction."
  [o property individual]
  (.getOWLObjectHas-Value ontology-data-factory
                          (ensure-object-property o property)
                          (ensure-individual o individual)))

(.addMethod has-value :object object-has-value)

(defmontfn has-self
  "Returns an OWL has self restriction."
  [o property]
  (.getOWLObjectHasSelf ontology-data-factory
                        (ensure-object-property o property)))

(def
  ^{:private true} owl-class-handlers
  {:subclass add-subclass
   :equivalent add-equivalent
   :disjoint add-disjoint
   :annotation add-annotation
   :haskey add-has-key
   :comment add-comment
   :label add-label})

(defdontfn owl-class-explicit
  "Creates a class in the current ontology.
Frames is a map, keyed on the frame name, value a list of items (of other
lists) containing classes. This function has a rigid syntax, and the more
flexible 'owl-class' is normally prefered. However, this function should be
slightly faster.
"
  [o name frames]
  (let [o (or (first (get frames :ontology))
              o)
        class (ensure-class o name)]
    (do
      ;; add the class
      (add-class o class)
      ;; add an name annotation
      (add-a-name-annotation o class name)
      ;; apply the handlers to the frames
      (doseq [[k f] owl-class-handlers]
        (f o class (get frames k)))
      ;; return the class object
      class)))

(defdontfn owl-class
  "Creates a new class in the current ontology. See 'defclass' for
full details."
  [ontology name & frames]
  (owl-class-explicit
   ontology name
   (util/check-keys
    (util/hashify frames)
    (list*
     :ontology :name
     (keys owl-class-handlers)))))

(defmacro defclass
  "Define a new class. Accepts a set number of frames, each marked
by a keyword :subclass, :equivalent, :annotation, :name, :comment,
:label or :disjoint. Each frame can contain an item, a list of items or any
combination of the two. The class object is stored in a var called classname."
  [classname & frames]
  `(let [string-name# (name '~classname)
         class# (tawny.owl/owl-class string-name# ~@frames)]
     (intern-owl ~classname class#)))

(defdontfn disjoint-classes-list
  "Makes all elements in list disjoint.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre [(seq? list)
         (> (count list) 1)]}
  (let [classlist
        (doall
         (map
          (fn [x]
            (ensure-class o x))
          list))]
    (add-axiom o
               (.getOWLDisjointClassesAxiom
                ontology-data-factory
                (into-array OWLClassExpression
                            classlist)))))

(defdontfn disjoint-classes
  "Makes all the arguments disjoint.
All arguments must be an instance of OWLClassExpression."
  [o & list]
  (disjoint-classes-list o list))

(defbdontfn add-type
  {:doc "Adds CLAZZES as a type to individual to current ontology
or ONTOLOGY if present."
   :arglists '([individual & clazzes] [o individual & clazzes])}
  [o individual clazz]
  (add-axiom o
             (.getOWLClassAssertionAxiom
              ontology-data-factory
              (ensure-class o clazz)
              individual)))

(defbdontfn add-fact
  {:doc "Add FACTS to an INDIVIDUAL in the current ontology or
  ONTOLOGY if present. Facts are produced with `fact' and `fact-not'."
   :arglists '([individual & facts] [ontology individual & facts])}
  [o individual fact]
  (add-axiom o
             (fact individual)))

(defmulti get-fact guess-type-args)
(defmulti get-fact-not guess-type-args)

(defmontfn fact
  "Returns a fact asserting a relationship with PROPERTY toward an
individual TO."
  [o property to]
  (fn fact [from]
    (get-fact o property from to)))

(defmontfn fact-not
  "Returns a fact asserting the lack of a relationship along PROPERTY
toward an individual TO."
  [o property to]
  (fn fact-not [from]
    (get-fact-not o property from to)))

(defmontfn object-get-fact
  "Returns an OWL Object property assertion axiom."
  [_ property from to]
  (.getOWLObjectPropertyAssertionAxiom
   ontology-data-factory
   property from to))

(.addMethod get-fact :object object-get-fact)

(defmontfn object-get-fact-not
  "Returns a negative OWL Object property assertion axiom."
  [_ property from to]
  (.getOWLNegativeObjectPropertyAssertionAxiom
   ontology-data-factory
   property from to))

(.addMethod get-fact-not :object object-get-fact-not)

(defdontfn
  add-same
  {:doc "Adds all arguments as the same individual to the current ontology
or to ONTOLOGY if present."
   :arglists '([ontology & individuals] [& individuals])}
  [o & individuals]
  (let [individuals (filter (comp not nil?) (flatten individuals))]
    (when individuals
      (add-axiom o
                 (.getOWLSameIndividualAxiom
                  ontology-data-factory
                  (set individuals))))))

(defmontfn add-different
  {:doc "Adds all arguments as different individuals to the current
  ontology unless first arg is an ontology in which case this is used"}
  [o & individuals]
  (let [individuals (filter (comp not nil?) (flatten individuals))]
    (when individuals
      (add-axiom o
                 (.getOWLDifferentIndividualsAxiom
                  ontology-data-factory
                  (set individuals))))))


;; need to support all the different frames here...
;; need to use hashify -- need to convert to handlers
(def
  ^{:private true}
  individual-handlers
  {:type add-type
   :fact add-fact
   :same add-same
   :different add-different})

(defdontfn individual-explicit
  "Returns a new individual."
  [o name & frames]
  (let [hframes
        (util/check-keys
         (util/hashify frames)
         [:type :fact :same :different :ontology])
        o (or (first (:ontology hframes))
              o)
        individual (ensure-individual o name)]
    (add-axiom o
               (.getOWLDeclarationAxiom ontology-data-factory individual))
    (add-a-name-annotation o individual name)
    (doseq [[k f] individual-handlers]
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

(defmacro defindividual
  "Declare a new individual."
  [individualname & frames]
  `(let [string-name# (name '~individualname)
         individual# (tawny.owl/individual string-name# ~@frames)]
     (intern-owl ~individualname individual#)))

(load "owl_data")

(defn- var-get-maybe
  "Given a var return it's value, given a value return the value."
  [var-maybe]
  (if (var? var-maybe)
    (var-get var-maybe)
    var-maybe))

(defdontfn as-disjoint
  {:doc "All entities declared in scope are declared as disjoint.
See also 'as-subclasses'."
   :arglists '([ontology & classes] [& classes])}
  [o & classes]
  (disjoint-classes-list
   o (map var-get-maybe classes)))

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
  (let [options (into #{} (take-while keyword? rest))
        subclasses
        (map
         var-get-maybe
         (flatten (drop-while keyword? rest)))]
    ;; first we deal with subclasses
    (util/domap
     #(add-subclass o % superclass)
     subclasses)
    (when
        (:disjoint options)
      (disjoint-classes-list o subclasses))
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

;; predicates
(defdontfn direct-superclasses
  "Returns the direct superclasses of name.
Name can be either a class or a string name. Returns a list of class
expressions."
  [o name]
  (let [clz (ensure-class o name)]
    ;; general Class expressions return empty
    (if (instance? OWLClass clz)
      (.getSuperClasses clz o)
      ())))

;; does the OWL API really not do this for me?
(defn- superclasses-1
  "Returns all subclasses of all classes in classlist."
  [o classlist]
  ;; if there are no subclasses return empty list
  (if (= 0 (count classlist))
    (list)
    (concat (list (first classlist))
            ;; can't use recur, not in tail position
            (superclasses-1 o
                            (rest classlist))
            (superclasses-1 o
                            (direct-superclasses o (first classlist))))))

(defdontfn superclasses
  "Return all subclasses of class"
  [o class]
  (superclasses-1 o (direct-superclasses o class)))

(defdontfn superclass?
  "Returns true is name has superclass as a superclass"
  [o name superclass]
  (let [namecls (ensure-class o name)
        superclasscls (ensure-class o superclass)]
    (some #(.equals superclasscls %) (superclasses o name))))

(defdontfn direct-subclasses
  "Returns the direct subclasses of name."
  [o name]
  (let [clz (ensure-class o name)]
    (if (instance? OWLClass clz)
      (.getSubClasses (ensure-class o  name)
                      o)
      ())))

(defn- subclasses-1
  "Returns all subclasses of all classes in classlist."
  [o classlist]
  ;; if there are no subclasses return empty list
  (if (= 0 (count classlist))
    (list)
    (concat (list (first classlist))
            ;; can't use recur, not in tail position
            (subclasses-1 o
                          (rest classlist))
            (subclasses-1 o
                          (direct-subclasses o (first classlist))))))

(defdontfn subclasses
  "Return all subclasses of class"
  [o class]
  (subclasses-1 o (direct-subclasses o class)))

(defdontfn subclass?
  "Returns true if name has subclass as a subclass"
  [o name subclass]
  (let [namecls (ensure-class o name)
        subclasscls (ensure-class o subclass)]
    (some #(.equals subclasscls %) (subclasses o name))))

(defdontfn disjoint?
  "Returns t iff classes are asserted to be disjoint."
  [o a b]
  (contains?
   (.getDisjointClasses a o)
   b))

(defdontfn equivalent?
  "Returns t iff classes are asserted to be equivalent."
  [o a b]
  (contains?
   (.getEquivalentClasses a o) b))

(defdontfn inverse?
  "Returns t iff properties are asserted to be inverse"
  [o p1 p2]
  (contains?
   (.getInverses p1 o) p2))
;; some test useful macros

;; currently doesn't support an ontology argument
;; modified from with-open
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
     (= (count bindings) 0)
     `(do
        ~@body)
     (symbol? (bindings 0))
     `(with-ontology ~o
        (let ~(subvec bindings 0 2)
          (with-probe-entities ~o
            ~(subvec bindings 2)
            ;; try block just so we can use finally
            (try
              ~@body
              (finally
                (tawny.owl/remove-entity ~o ~(bindings 0)))))))
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
     (= (count bindings) 0)
     `(do ~@body)
     (symbol? (bindings 0))
     `(with-ontology ~o
        (let ~(subvec bindings 0 2)
          (with-probe-axioms ~o
            ~(subvec bindings 2)
            ;; try block just so we can use finally
            (try
              ~@body
              (finally
                (tawny.owl/remove-axiom ~o ~(bindings 0)))))))
     :else
     (throw (IllegalArgumentException.
             "with-probe-axioms only allows Symbols in bindings")))))

(defn owl-thing
  "Returns OWL thing."
  []
  (.getOWLThing ontology-data-factory))

(defn owl-nothing
  "Returns OWL nothing."
  []
  (.getOWLNothing ontology-data-factory))

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
This is a convienience macro and is lexically scoped."
  [prefix & body]
  (let [newbody
        (alter-all-symbol-after-def-form
         (partial prefix-symbol prefix)
         body)]
    `(do ~@newbody)))

(defmacro with-suffix
  "Adds a suffix to all defclass macros in scope.
This is a convienience macro and is lexically scoped."
  [suffix & body]
  (let [newbody
        (alter-all-symbol-after-def-form
         (partial suffix-symbol suffix)
         body)]
    `(do ~@newbody)))

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
  (fn [owlentity & _] (class owlentity)))

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

(defmacro defrefineto
  "Takes an existing definition, add more frames.
The first argument should be a symbol that will hold the

See also 'refine'.
"
  [symbol & args]
  `(def
     ~(with-meta symbol
        (assoc (meta symbol)
          :owl true))
     (tawny.owl/refine ~@args)))

(defmacro defrefine
  "Takes an existing definition, add more frames.

The first element should be a namespace qualified symbol. The
unqualifed part of this will be used in the current namespace.

See also 'refine'
"
  [symb & args]
  (let [newsymbol#
        (symbol (name symb))]
    `(def
       ~(with-meta newsymbol#
          (assoc (meta newsymbol#)
            :owl true))
       (tawny.owl/refine ~symb ~@args))))

(defmacro defcopy
  "Takes an existing definition from another namespace and copies it into the
current namespace with no changes in semantics. This can be useful for
convienience, where one namespace should contain all the OWLObjects of
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
