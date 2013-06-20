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
                                 OWLDataPropertyExpression OWLLiteral
                                 )
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

;; the current ontology provides our main mutable state. Strictly, we don't
;; need to do this, but the alternative would be passing in an ontology to
;; almost every call, or running everything inside a binding. Painful.
(def
  ^{:dynamic true
    :doc "The currently bound ontology. If this is not set, then the current
ontology is expected to be bound to the current namespace with 'defontology'
or similar macros. Normally, when set, this is set with the 'with-ontology'
macro." }
  *current-bound-ontology* nil)

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
      (util/run-hook remove-ontology-hook o))))


(declare add-annotation)
(declare owlcomment)
(declare versioninfo)
(declare ontology-options)
(declare seealso)

(defn ontology
  "Returns a new ontology. See 'defontology' for full description."
  [& args]
  (let [options (apply hash-map args)
        iri (IRI/create (:iri options))]
    (remove-ontology-maybe
     (OWLOntologyID. iri))

    (let [jontology
          (.createOntology owl-ontology-manager iri)

          ontology-format
          (.getOntologyFormat
           owl-ontology-manager jontology)]

      ;; iri gen options needs to be remembered
      (when-let [iri-gen (:iri-gen options)]
        (dosync
         (alter (ontology-options jontology)
                merge {:iri-gen iri-gen})))

      ;; put prefix into the prefix manager
      ;; this isn't a given -- the prefix manager being returned by default
      ;; returns true for isPrefixOWLOntologyFormat, so we can do this, but
      ;; strictly we are depending on the underlying types.
      (when-let [prefix (:prefix options)]
        (.setPrefix ontology-format
                    prefix
                    (.toString iri)))


      ;; add annotations to the ontology
      (add-annotation
       jontology
       (filter (comp not nil?)
               (flatten
                (list
                 (:annotation options)
                 (when-let [s (:seealso options)]
                   (seealso jontology s))
                 (when-let [c (:comment options)]
                   (owlcomment jontology c))
                 (when-let [v (:versioninfo options)]
                   (versioninfo jontology v))))))
      jontology)))

;;
;; we use intern-owl here because we need to attach the ontology to the
;; metadata for the symbol. With def this isn't possible because we need
;; to generate the symbol at compile time, when we don't have the
;; ontology yet. &form is an implicit variable for macro's, and carries
;; the line and column metadata which otherwise we loose. This is a
;; compiler detail, but there is no other way, as far as I can tell.
;;
;; However, we still need to declare at this point, or we get into
;; trouble in macros such as "as-disjoint" which check symbols at compile
;; time, and cannot determine that the symbol will exist at run time,
;; once the intern has run.
;;
;; This all seems horribly messy, and I am not sure that it is all true;
;; after all I can do this?
;; (let [n (tawny.owl/owlclass "y")]
;;                 (def ^{:tmp n} g n))
;;
;; However, an attempt to try this using this sort of thing fails: the
;; metadata disappears away, I think because the reader is doing something
;; special with it. Regardless it does not appear in the expansion.
;;
;; (def ^{:owl true
;;        :doc
;;        (tawny.util.CallbackString.
;;         (partial tawny.repl/fetch-doc ontology#))}
;;   ~ontname ontology#)
;;
;; Also have tried calling the "ontology" function at compile time, and then
;; generating the appropriate form.
;;
;; This sounds reasonable, but doesn't work either, because valn# is the FORM
;; and not the evaluation of it.
;;
;; (defmacro defthing
;;   [name val]
;;   (let [valn# (identity val)]
;;     `(def
;;        ~(vary-meta
;;          name
;;          merge
;;          {:owl true
;;           :val valn#})
;;        ~valn#)
;;      ))
;;
;; So, I think that this is about the best I can do. The ultimate solution has
;; to be changes to the way the doc lookup works. Putting an entity into it's
;; own metadata makes no sense. It would be much cleaner if the doc function
;; checked the *value* of a var, to see if it implements a documentation
;; protocol, and if not uses the :doc metadata.
(defmacro defontology
  "Define a new ontology with `name'.

The following keys must be supplied.
:iri -- the IRI for the new ontology
:prefix -- the prefix used in the serialised version of the ontology
"
  [name & body]
  `(do
     (let [ontology# (ontology ~@body)
           var#
           (def
             ~(with-meta name
                (assoc (meta name)
                  :owl true))
             ontology#)]
       (tawny.owl/ontology-to-namespace ontology#)
       var#
       )))

(defn ontology-to-namespace
  "Sets the current ontology as defined by `defontology'"
  [o]
  (dosync (ref-set
           ontology-for-namespace
           (merge @ontology-for-namespace
                  {*ns* o}))))


;; ontology options -- additional knowledge that I want to attach to each
;; ontology,  but which gets junked when the ontology does.
(def ^{:doc "Ontology options. A map on a ref for each ontology"}
  ontology-options-atom (atom {}))

(defmacro defnwithfn [name function & body]
  `(let [vr# (defn ~name ~@body)]
     (alter-var-root
      vr#
      (fn [f#]
        (fn [& args#]
          (apply ~function f# args#))))
     vr#))

(defonce
  ^{:doc "Hook called when the default ontology is used"}
  default-ontology-hook (util/make-hook))

(declare get-current-ontology)
(defn default-ontology [f & args]
  (if (instance?
       org.semanticweb.owlapi.model.OWLOntology (first args))
    (apply f args)
    (do
      ;; a call back so we can block this if we choose
      (util/run-hook default-ontology-hook)
      (apply f (get-current-ontology) args))))

(defmacro defontfn
  "Like defn, but automatically adds the current-ontology to the args
if the first arg is not an ontology."
  [name & body]
  `(defnwithfn ~name #'default-ontology
     ~@body))

(defn broadcast-ontology
  [f & args]
  (apply default-ontology
    (fn [o & narg]
      (doall
       (map (partial f o (first narg))
            (filter identity
                    (flatten
                     (rest narg))))))
    args))

(defmacro defbontfn
  [name & body]
  `(defnwithfn ~name #'broadcast-ontology
     ~@body))

;; basically, this does the same thing as defontfn, but is easier I think.
;; bind to
(defmacro with-ontology
  "Sets the default ontology for all operations inside its dynamic scope."
 [o & body]
  `(binding [tawny.owl/*current-bound-ontology* ~o]
     ~@body))

(defn ontology-first-maybe
  "Given a function f, returns a function which if the first arg is an
OWLOntology sets it as the current ontology, then calls f with the remain
args, or else calls f."
  [f]
  (fn ontology-first-maybe
    [& args]
    (if (and
         ;; balk if nil
         (seq args)
         ;; ontology first
         (instance? OWLOntology
                        (first args)))
     (with-ontology (first args)
        (apply f (rest args)))
      (apply f args))))

(def
  ^{:doc "Transform a two arg function so that if the first element is an
  ontology set it as the current, then drop this parameter. Then call the next
  parameter repeatedly against all the remaining parameters."}
  ontology-vectorize
  (comp ontology-first-maybe util/vectorize))


;; return options for ontology -- lazy (defn get-ontology-options [ontology])
(defontfn ontology-options
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

(util/add-hook remove-ontology-hook
               (fn [o]
                 (dosync
                  (swap! ontology-options-atom
                         dissoc o))))


(defn iri
  "Returns an IRI object given a string. Does no transformation on the
string; use 'iriforname' to perform ontology specific expansion"
  [name]
  (IRI/create name))

(declare get-iri)
(defontfn iriforname
  "Returns an IRI object for the given name.

This is likely to become a property of the ontology at a later date, but at
the moment it is very simple."
  [o name]
  (if-let [iri-gen (:iri-gen (deref (ontology-options o)))]
    (iri-gen name)
    (iri (str (get-iri o) "#" name))))

(defn check-entity-set
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

(defontfn entity-for-iri
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
   (check-entity-set
    (apply
     clojure.set/union
     (map #(.getEntitiesInSignature % iri true)
          (vals @ontology-for-namespace)))
    iri)))

(defontfn entity-for-string
  "Returns the OWLObject for a given string.

See 'entity-for-iri' for more details."
  [o string]
  (or (entity-for-iri o (iriforname o string))
      ;; string name somewhere?
      (entity-for-iri o (iri string))))

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

(defn get-current-ontology
  "Gets the current ontology"
  ([]
     (get-current-ontology *ns*))
  ([ns]
     ;; if current ontology is inside a binding
     (or *current-bound-ontology*
         ;; so use the namespace bound one
         (get @ontology-for-namespace ns)
         ;; so break
         (throw (IllegalStateException. "Current ontology has not been set")))))


(defontfn get-iri
  "Gets the IRI for the given ontology, or the current ontology if none is given"
  [o]
  (.getOntologyIRI
   (.getOntologyID o)))

(defn get-current-iri[]
  "DEPRECATED: Use 'get-iri' instead. "
  {:deprecated "0.8"}
  (get-iri))

(defontfn get-prefix
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

(defn get-current-prefix []
  "Gets the current prefix"
  {:deprecated "0.8"}
  (get-prefix))

(defontfn save-ontology
  "Save the current 'ontology' in the file or `filename' if given.
If no ontology is given, use the current-ontology"
  ([o filename]
     (save-ontology o filename (ManchesterOWLSyntaxOntologyFormat.)
                    (str "## This file was created by Clojure-OWL\n"
                         "## It should not be edited by hand\n" )))
  ([o filename format]
     (save-ontology o filename format ""))
  ([o filename format prepend]
     (let [file (new File filename)
           output-stream (new FileOutputStream file)
           file-writer (new PrintWriter output-stream)
           existingformat (.getOntologyFormat owl-ontology-manager
                                              o)
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
       (.setPrefix this-format (get-current-prefix)
                   (str (.toString (get-current-iri)) "#"))
       (.saveOntology owl-ontology-manager o
                      this-format output-stream))))

(defontfn guess-type
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
  {:pre [(instance? OWLOntology o)]}
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
     ;; if an IRI, see if it is the current ontology
     (instance? IRI entity)
     (guess-type o (entity-for-iri o entity))
     ;; string name in current ontology?
     (string? entity)
     (guess-type o (entity-for-string o entity))
     ;; owl individuals tell us nothing, cause we still don't know!
     (instance? OWLIndividual entity)
     nil
     (number? entity)
     nil
     (nil? entity)
     nil
     :default
     (throw (IllegalArgumentException.
             (str "Cannot guess this type:" entity))))))

(defontfn guess-individual-literal
  [o entity]
  (cond
   (coll? entity)
   (some guess-individual-literal entity)
   (instance? OWLIndividual entity)
   :individual
   (instance? OWLLiteral entity)
   :literal
   (instance? IRI entity)
   (guess-individual-literal
    (entity-for-iri o entity))
   (string? entity)
   (guess-individual-literal
    (entity-for-string o entity))
   :default
   (throw (IllegalArgumentException.
           (str "Cannot tell if this is individual or literal:" entity)))))


(defontfn
  ^{:private true}
  get-create-object-property
  "Creates an OWLObjectProperty for the given name."
  [o name]
  (.getOWLObjectProperty ontology-data-factory
                         (iriforname o name)))

(defontfn
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
   (get-create-object-property o prop)
   :default
   (throw (IllegalArgumentException.
           (str "Expecting an object property. Got: " prop)))))

(defontfn
  ^{:private true}
  get-create-class
  "Returns an OWL class."
  [o name]
  (.getOWLClass ontology-data-factory
                (iriforname o name)))

(defontfn ensure-class [o clz]
  "If clz is a String return a class of with that name,
else if clz is a OWLClassExpression add that."
  (cond
   (fn? clz)
   (ensure-class o (clz))
   (instance? org.semanticweb.owlapi.model.OWLClassExpression clz)
   clz
   (instance? IRI clz)
   (.getOWLClass ontology-data-factory clz)
   (string? clz)
   (get-create-class o clz)
   true
   (throw (IllegalArgumentException.
           (str "Expecting a class. Got: " clz)))))

(defontfn add-axiom
  "Adds an axiom from the given ontology, or the current one."
  [o axiom]
  (.applyChange owl-ontology-manager
                (AddAxiom. o axiom))
  axiom)

(defontfn remove-axiom
  "Removes an axiom from the given ontology, or the current one."
  [o axiom]
  (.applyChange owl-ontology-manager
                (RemoveAxiom. o axiom))
  axiom)

(defontfn remove-entity
  "Remove from the ontology an entity created and added by
owlclass, defclass, objectproperty or defoproperty. Entity is the value
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

(defbontfn
  add-subclass
  {:doc "Adds one or more subclass to name in ontology."
   :arglists '([name & subclass] [ontology name & subclass])}
  [o name subclass]
  (add-axiom o
             (.getOWLSubClassOfAxiom
              ontology-data-factory
              (ensure-class o name)
              (ensure-class o subclass))))

(defbontfn add-equivalent
  {:doc "Adds an equivalent axiom to the ontology."
   :arglists '([name equivalent] [ontology name equivalent])
   }
  [o name equivalent]
  (add-axiom o
             (.getOWLEquivalentClassesAxiom
              ontology-data-factory
              (ensure-class o name)
              (ensure-class o equivalent))))

(defbontfn add-disjoint
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

(defontfn add-disjoint-union
  "Adds a disjoint union axiom to all subclasses."
  [o clazz subclasses]
  (let [ensured-subclasses
        (util/domap #(ensure-class %) subclasses)
        ]
    (list
     (add-axiom o
      (.getOWLDisjointUnionAxiom
       ontology-data-factory
       (ensure-class o clazz)
       (java.util.HashSet. ensured-subclasses))))))

(defontfn add-class
  "Adds a class to the ontology."
  [o name]
  (add-axiom o
             (.getOWLDeclarationAxiom
              ontology-data-factory
              (ensure-class o name))))

(declare ensure-data-property)
;; a class can have only a single haskey, so ain't no point broadcasting this.
(defontfn add-haskey
  [o class propertylist]
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
                         (into #{} propertylist)))))

(defbontfn add-domain
  "Adds all the entities in domainlist as domains to a property."
  [o property domain]
  (add-axiom o
             (.getOWLObjectPropertyDomainAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-class o domain))))

(defbontfn add-range
  "Adds all the entities in rangelist as range to a property."
  [o property range]
  (add-axiom o
         (.getOWLObjectPropertyRangeAxiom
          ontology-data-factory
          (ensure-object-property o property)
          (ensure-class o range))))

(defbontfn add-inverse
  "Adds all the entities in inverselist as inverses to a property."
  [o property inverse]
  (add-axiom o
         (.getOWLInverseObjectPropertiesAxiom
          ontology-data-factory
          (ensure-object-property o property)
          (ensure-object-property o inverse))))


(defbontfn add-superproperty
  "Adds all items in superpropertylist to property as
a superproperty."
  [o property superproperty]
  (add-axiom o
             (.getOWLSubObjectPropertyOfAxiom
              ontology-data-factory
              (ensure-object-property o property)
              (ensure-object-property o superproperty))))

;; broadcasts specially
(defontfn add-subpropertychain
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


;; Really it would make more sense to use keywords, but this breaks the
;; groupify function which expects alternative keyword value args. The
;; approach of using strings and symbol names here is scary -- if someone does
;; (defclass transitive) for example, it's all going to break. I don't think
;; that the const does what it might
(def ^:const transitive "transitive")
(def ^:const functional "functional")
(def ^:const inversefunctional "inversefunctional")
(def ^:const symmetric "symmetric")
(def ^:const asymmetric "asymmetric")
(def ^:const reflexive "reflexive")
(def ^:const irreflexive "irreflexive")


(def
  ^{:private true}
  charfuncs
  {transitive #(.getOWLTransitiveObjectPropertyAxiom %1 %2)
   functional #(.getOWLFunctionalObjectPropertyAxiom %1 %2)
   inversefunctional #(.getOWLInverseFunctionalObjectPropertyAxiom %1 %2)
   symmetric #(.getOWLSymmetricObjectPropertyAxiom %1 %2)
   asymmetric #(.getOWLAsymmetricObjectPropertyAxiom %1 %2)
   irreflexive #(.getOWLIrreflexiveObjectPropertyAxiom %1 %2)
   reflexive #(.getOWLReflexiveObjectPropertyAxiom %1 %2)
   })

(defbontfn add-characteristics
  "Add a list of characteristics to the property."
  [o property characteristic]
  (when-not (get charfuncs characteristic)
    (throw (IllegalArgumentException.
            "Characteristic is not recognised:" characteristic)))
  (add-axiom o
             ((get charfuncs characteristic)
              ontology-data-factory (ensure-object-property property))))

(def
  ^{:doc "Frames to add to all new classes."
    :dynamic true
    :private true}
  *default-frames* nil)

(def
  ^{:doc "Axioms we have added recently"
    :dynamic true}
  recent-axiom-list
  nil)

;; object properties
(defn objectproperty-explicit
  "Returns an objectproperty. This requires an hash with a list
value for each frame."
  [name {:keys [domain range inverseof subpropertyof
                characteristics subpropertychain ontology]}]
  (let [o (or (first ontology)
              (get-current-ontology))
        property (ensure-object-property o name)
        axioms
        (concat
         (list (add-axiom o
                (.getOWLDeclarationAxiom
                 ontology-data-factory property)))
         (add-domain o property domain)
         (add-range o property range)
         (add-inverse o property inverseof)
         (add-superproperty o property subpropertyof)
         (add-characteristics o property characteristics)
         (add-subpropertychain o property subpropertychain)
         )]
    ;; store classes if we are in an inverse binding
    (when (seq? recent-axiom-list)
      (set! recent-axiom-list
            (concat (list property) recent-axiom-list)))
    property))


(defn objectproperty
  "Returns a new object property in the current ontology."
  [name & frames]
  (objectproperty-explicit
   name
   (util/check-keys
    (merge-with concat
                (util/hashify frames)
                *default-frames*)
    [:domain :range :inverseof :subpropertyof :characteristics
     :ontology])))

(defmacro defoproperty
  "Defines a new object property in the current ontology."
  [property & frames]
  `(let [property-name# (name '~property)
         property# (tawny.owl/objectproperty property-name# ~@frames)]
     (def ~(with-meta property
             (assoc (meta name)
               :owl true))
       property#)))

(defontfn
  guess-type-args
  {:private true}
  [o & args]
  (guess-type o args))

(defontfn guess-individual-literal-args
  {:private true}
  [o & args]
  (guess-individual-literal o args))

;; multi methods for overloaded entities. We guess the type of the arguments,
;; which can be (unambiguous) OWLObjects, potentially ambiguous IRIs or
;; strings. If we really can tell, we guess at objects because I like objects
;; better.
(defmulti owlsome #'guess-type-args)
(defmulti only #'guess-type-args)
(defmulti someonly #'guess-type-args)
(defmulti owland #'guess-type-args)
(defmulti owlor #'guess-type-args)
(defmulti owlnot #'guess-type-args)
(defmulti exactly #'guess-type-args)
(defmulti oneof #'guess-individual-literal-args)
(defmulti atleast #'guess-type-args)
(defmulti atmost #'guess-type-args)

;; short cuts for the terminally lazy. Still prefix!
(def && owland)
(def || owlor)
(def ! owlnot)

;; "long cuts" for consistency with some
(def owlonly only)


;; restrictions! name clash -- we can do nothing about this, so accept the
;; inconsistency and bung owl on the front.
(defbontfn object-some
  {:doc "Returns an OWL some values from restriction."
    :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectSomeValuesFrom
   ontology-data-factory
   (ensure-object-property o property)
   (ensure-class o class)))

(.addMethod owlsome :object object-some)

(defbontfn object-only
  {:doc "Returns an OWL all values from restriction."
   :arglists '([property & clazzes] [ontology property & clazzes])}
  [o property class]
  (.getOWLObjectAllValuesFrom
   ontology-data-factory
   (ensure-object-property o property)
   (ensure-class o class)))

(.addMethod only :object object-only)


;; union, intersection
(defontfn object-and
  "Returns an OWL intersection of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owland must have at least two classes")))

    (.getOWLObjectIntersectionOf
     ontology-data-factory
     (java.util.HashSet.
      (util/domap
       #(ensure-class o %)
       ;; flatten list for things like owlsome which return lists
       classes)))))

;; add to multi method
(.addMethod owland :object object-and)

(defontfn object-or
  "Returns an OWL union of restriction."
  [o & classes]
  (let [classes (flatten classes)]
    (when (> 1 (count classes))
      (throw (IllegalArgumentException. "owlor must have at least two classes")))

    (.getOWLObjectUnionOf
     ontology-data-factory
     (java.util.HashSet.
      (util/domap #(ensure-class o %)
             (flatten classes))))))

(.addMethod owlor :object object-or)

;; lots of restrictions return a list which can be of size one. so all these
;; functions take a list but ensure that it is of size one.
(defontfn object-not
  "Returns an OWL complement of restriction."
  [o & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectComplementOf
   ontology-data-factory
   (ensure-class o (first (flatten class)))))

(.addMethod owlnot :object object-not)

(defontfn object-someonly
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

(.addMethod someonly :object object-someonly)

;; cardinality
(defontfn object-atleast
  "Returns an OWL atleast cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectMinCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod atleast :object object-atleast)

(defontfn object-atmost
  "Returns an OWL atmost cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectMaxCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod atmost :object object-atmost)


(defontfn object-exactly
  "Returns an OWL exact cardinality restriction."
  [o cardinality property & class]
  {:pre [(= 1
            (count (flatten class)))]}
  (.getOWLObjectExactCardinality
   ontology-data-factory cardinality
   (ensure-object-property o property)
   (ensure-class o (first (flatten class)))))

(.addMethod exactly :object object-exactly)

(declare ensure-individual)
(defn object-oneof
  "Returns an OWL one of property restriction."
  [o & individuals]
  (.getOWLObjectOneOf
   ontology-data-factory
   (java.util.HashSet.
    (doall
     (map (partial ensure-individual o)
          (flatten individuals))))))

(.addMethod oneof :individual object-oneof)

(defontfn hasvalue [o property individual]
  (.getOWLObjectHasValue
   (ensure-object-property o property)
   (ensure-individual o individual)))

(defontfn hasself [o property]
  (.getOWLObjectHasSelf
   (ensure-object-property o property)))


;; annotations
(defn- add-a-simple-annotation
  [o named-entity annotation]
  (let [axiom
        (.getOWLAnnotationAssertionAxiom
         ontology-data-factory
         (.getIRI named-entity) annotation)]
    (add-axiom o axiom)))

(defn- add-an-ontology-annotation
  [o annotation]
  (.applyChange
   owl-ontology-manager
   (AddOntologyAnnotation. o annotation)))


(defontfn add-annotation
  ([o named-entity annotation-list]
     (doall
      (for [n annotation-list]
        (add-a-simple-annotation o named-entity n))))
  ([o annotation-list]
     (doall
      (for [n annotation-list]
        (add-an-ontology-annotation o n)))))

(defn- ensure-annotation-property [o property]
  "Ensures that 'property' is an annotation property,
converting it from a string or IRI if necessary."
  (cond
   (instance? OWLAnnotationProperty property)
   property
   (instance? IRI property)
   (.getOWLAnnotationProperty
    ontology-data-factory property)
   (instance? String property)
   (ensure-annotation-property
    o
    (iriforname o property))
   :default
  (throw (IllegalArgumentException.
           (format "Expecting an OWL annotation property: %s" property)))))

(defontfn annotation
  "Creates a new annotation property."
  ([o annotation-property literal]
     (annotation o annotation-property literal "en"))
  ([o annotation-property literal language]
     (.getOWLAnnotation
      ontology-data-factory
      (ensure-annotation-property o annotation-property)
      (.getOWLLiteral ontology-data-factory literal language))))

(defbontfn add-super-annotation
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
(def labelproperty
  (.getRDFSLabel ontology-data-factory))

(defontfn label
  [o & args]
  (apply annotation o labelproperty args))

(def owlcommentproperty
  (.getRDFSComment ontology-data-factory))

(defontfn owlcomment
  [o & args]
  (apply annotation o owlcommentproperty args))

(def isdefinedbyproperty
  (.getRDFSIsDefinedBy ontology-data-factory))

(defontfn isdefinedby
  [o & args]
  (apply annotation o isdefinedbyproperty args))

(def seealsoproperty
  (.getRDFSSeeAlso ontology-data-factory))

(defontfn seealso
  [o & args]
  (apply annotation o
         seealsoproperty args))

(def backwardcompatiblewithproperty
  (.getOWLBackwardCompatibleWith ontology-data-factory))

(defontfn backwardcompatiblewith
  [o & args]
  (apply annotation o
              backwardcompatiblewithproperty
              args))

(def incompatiblewithproperty
  (.getOWLIncompatibleWith ontology-data-factory))

(defontfn incompatiblewith
  [o & args]
  (apply annotation o
         incompatiblewithproperty
         args))

(def versioninfoproperty
  (.getOWLVersionInfo ontology-data-factory))

(defontfn versioninfo
  [o & args]
  (apply annotation o
              versioninfoproperty
              args))

(def deprecatedproperty
  (.getOWLDeprecated ontology-data-factory))

(defontfn deprecated
  [o & args]
  (apply annotation o
         deprecatedproperty
         args))

(defn annotation-property-explicit
  "Add this annotation property to the ontology"
  [property
   {:keys [comment label supers annotation ontology]}]
  (let [o
        (or (first ontology)
            (get-current-ontology))
        property-object
        (ensure-annotation-property o property)
        ]
    ;; add the property
    (.addAxiom owl-ontology-manager
               o
               (.getOWLDeclarationAxiom
                ontology-data-factory
                property-object))

    (when comment
      (add-annotation o
                      property-object
                      (list (owlcomment
                             (first comment)))))

    (when label
      (add-annotation o
                      property-object
                      (list (tawny.owl/label
                             (first
                              label)))))

    (when supers
      (add-super-annotation o
                            property-object supers))

    (add-annotation o property-object annotation)
    property-object))

(defn annotation-property
  "Creates a new annotation property."
  [name & frames]
  (annotation-property-explicit
   name
   (util/check-keys
    (util/hashify frames)
    [:ontology :annotation :label :comment :subproperty])))

(defn- get-annotation-property
  "Gets an annotation property with the given name."
  [o property]
  (.getOWLAnnotationProperty
   ontology-data-factory
   (iriforname o property)))

(defmacro defannotationproperty
  "Defines a new annotation property in the current ontology.
See 'defclass' for more details on the syntax."
  [property & frames]
  `(let [property-name# (name '~property)
         property#
         (tawny.owl/annotation-property property-name# ~@frames)]
     (def
       ~(vary-meta property
                   merge
                   {:owl true})
       property#)))

(defn owlclass-explicit
  "Creates a class in the current ontology.
Frames is a map, keyed on the frame name, value a list of items (of other
lists) containing classes. This function has a rigid syntax, and the more
flexible 'owlclass' is normally prefered. However, this function should be
slightly faster.
"
  [name {:keys [subclass equivalent disjoint annotation haskey
                comment label ontology]}]
  (let [o (or (first ontology)
                     (get-current-ontology))
        class (ensure-class o name)]
    ;; store classes if we are in a disjoint binding
    (when (seq? recent-axiom-list)
      (set! recent-axiom-list
            (concat (list class)
                    recent-axiom-list)))
    ;; create the class
    (do
      ;; add-class returns a single axiom -- concat balks at this
      (add-class o class)
      (add-subclass o class subclass)
      (add-equivalent o class equivalent)
      (add-disjoint o class disjoint)
      (add-annotation o class annotation)
      (when haskey
        (add-haskey class haskey))

      ;; change these to add to the annotation frame instead perhaps?
      (when comment
        (add-annotation class
                        (list (owlcomment
                               (first comment)))))

      (when label
          (add-annotation class
                        (list (tawny.owl/label
                               (first label)))))
      ;; return the class object
      class)))


(defn owlclass
  "Creates a new class in the current ontology. See 'defclass' for
full details."
  ([name & frames]
     (owlclass-explicit
      name
      (util/check-keys
       (merge-with
               concat
               (util/hashify frames)
               *default-frames*)
       [:subclass :equivalent :annotation
        :name :comment :label :disjoint
        :haskey]))))

(defmacro defclass
  "Define a new class. Accepts a set number of frames, each marked
by a keyword :subclass, :equivalent, :annotation, :name, :comment,
:label or :disjoint. Each frame can contain an item, a list of items or any
combination of the two. The class object is stored in a var called classname."
  [classname & frames]
  `(let [string-name# (name '~classname)
         class# (tawny.owl/owlclass string-name# ~@frames)]
     (def
       ~(vary-meta classname
                   merge
                   {:owl true})
       class#)))


(defontfn disjointclasseslist
  "Makes all elements in list disjoint.
All arguments must of an instance of OWLClassExpression"
  [o list]
  {:pre (seq? list)}
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

(defn disjointclasses
  "Makes all the arguments disjoint.
All arguments must be an instance of OWLClassExpression."
  [& list]
  (disjointclasseslist list))

(defn- get-create-individual
  "Returns an individual for the given name."
  [o individual]
  (.getOWLNamedIndividual ontology-data-factory
                          (iriforname o individual)))

(defn- ensure-individual [o individual]
  "Returns an INDIVIDUAL.
If INDIVIDUAL is an OWLIndividual return individual, else
interpret this as a string and create a new OWLIndividual."
  (cond (instance? org.semanticweb.owlapi.model.OWLIndividual individual)
        individual
        (instance? IRI individual)
        (.getOWLNamedIndividual ontology-data-factory
                                individual)
        (string? individual)
        (get-create-individual o individual)
        true
        (throw (IllegalArgumentException.
                (str "Expecting an Individual. Got: " individual)))))

(defbontfn add-type
  {:doc "Adds CLAZZES as a type to individual to current ontology
or ONTOLOGY if present."
   :arglists '([individual & clazzes] [o individual & clazzes])}
  [o individual clazz]
  (add-axiom o
   (.getOWLClassAssertionAxiom
    ontology-data-factory
    (ensure-class o clazz)
    individual)))

(defbontfn add-fact
  {:doc "Add FACTS to an INDIVIDUAL in the current ontology or
  ONTOLOGY if present. Facts are produced with `fact' and `fact-not'."
   :arglists '([individual & facts] [ontology individual & facts])}
  [o individual fact]
  (add-axiom o
   (fact individual)))


(defmulti getfact guess-type-args)
(defmulti getfactnot guess-type-args)

(defn fact
  "Returns a fact asserting a relationship with PROPERTY toward an
individual TO."
  [property to]
  (fn fact [from]
    (getfact property from to)))

(defontfn fact-not
  "Returns a fact asserting the lack of a relationship along PROPERTY
toward an individual TO."
  [o property to]
  (fn fact-not [from]
    (getfactnot o property from to)))


(defontfn object-getfact [o property from to]
  (.getOWLObjectPropertyAssertionAxiom
   ontology-data-factory
   property from to))

(.addMethod getfact :object object-getfact)

(defn object-getfactnot [o property from to]
  (.getOWLNegativeObjectPropertyAssertionAxiom
   ontology-data-factory
   property from to))

(.addMethod getfactnot :object object-getfactnot)

(defontfn
  add-same
  {:doc "Adds all arguments as the same individual to the current ontology
or to ONTOLOGY if present."
   :arglists '([ontology & individuals] [& individuals])}
  [o & individuals]
  (add-axiom o
   (.getOWLSameIndividualAxiom
    ontology-data-factory
    (set (flatten individuals)))))

(defontfn add-different
  {:doc "Adds all arguments as different individuals to the current
  ontology unless first arg is an ontology in which case this is used"}
  [o & individuals]
  (add-axiom o
   (.getOWLDifferentIndividualsAxiom
    ontology-data-factory
    (set (flatten individuals)))))

;; need to support all the different frames here...
;; need to use hashify
(defn individual
  "Returns a new individual."
  [name & frames]
  (let [hframes
        (util/check-keys
         (util/hashify frames)
         [:type :fact :same :different :ontology])
        o (or (first (:ontology hframes))
                     (get-current-ontology))
        individual (ensure-individual o name)]
    (add-axiom o
     (.getOWLDeclarationAxiom ontology-data-factory individual))
    (when (:type hframes)
      (add-type o individual
                (:type hframes)))
    (when (:fact hframes)
      (add-fact o individual
                (:fact hframes)))
    (when (:same hframes)
      (add-same o individual
                (:same hframes)))
    (when (:different hframes)
      (add-different o individual
                     (:different hframes)))
    individual))


(defmacro defindividual
  "Declare a new individual."
  [individualname & frames]
  `(let [string-name# (name '~individualname)
         individual# (tawny.owl/individual string-name# ~@frames)]
     (def
       ~(vary-meta individualname
                  merge
                  {:owl true})
       individual#)))

(load "owl_data")

;; owl imports
(defn owlimport
  "Adds a new import to the current ontology."
  ([o]
     (owlimport (get-current-ontology) o))
  ([ontology-into o]
     (.applyChange owl-ontology-manager
                   (AddImport. ontology-into
                               (.getOWLImportsDeclaration
                                ontology-data-factory
                                (get-iri o))))))
;; convienience macros
;; is this necessary? is as-disjoint-subclasses not enough?
(defmacro as-disjoint [& body]
  "All entities declared in scope are declared as disjoint.
See also 'as-subclasses'."
  `(do ;; delete all recent classes
     (binding [tawny.owl/recent-axiom-list '()]
       ;; do the body
       ~@body
       ;; set them disjoint if there is more than one. if there is only one
       ;; then it would be illegal OWL2. this macro then just shields the body
       ;; from any other as-disjoint statements.
       (when (< 1 (count tawny.owl/recent-axiom-list))
         (tawny.owl/disjointclasseslist
          tawny.owl/recent-axiom-list)))))

(defmacro as-inverse [& body]
  "The two properties declared in the dynamic scope of this macro
are declared as inverses."
  `(do
     (binding [tawny.owl/recent-axiom-list '()]
       ~@body
       (when-not (= (count tawny.owl/recent-axiom-list) 2)
         (throw
          (IllegalArgumentException.
           "Can only have two properties in as-inverse")))
       (tawny.owl/add-inverse
        (first tawny.owl/recent-axiom-list)
        (rest tawny.owl/recent-axiom-list))
       )))


;; specify default frames which should be merged with additional frames passed
;; in. place into a dynamic variable and then use (merge-with concat) to do
;; the business
(defmacro with-default-frames [frames & body]
  "Adds a standard frame to all entities declared within its scope.
This macro is lexically scoped."
  `(binding [tawny.owl/*default-frames*
             (tawny.util/hashify ~frames)]
     ~@body))


(defmacro as-disjoint-subclasses
  "All declared subclasses in body. Convienience
macro over 'as-subclasses'"
  [superclass & body]
  `(as-subclasses ~superclass :disjoint ~@body))

(defmacro as-subclasses [superclass & body]
  "All classes defined within body are given a superclass.
The first items in body can also be options.

:disjoint also sets the class disjoint.
:cover also makes the subclasses cover the superclass.

This macro is dynamically scoped."
  (let [options# (vec (take-while keyword? body))
        rest# (drop-while keyword? body)]
    `(binding [tawny.owl/recent-axiom-list '()]
       (with-default-frames [:subclass ~superclass]
        ~@rest#)
       (#'tawny.owl/subclass-options
        ~options#
        ~superclass
        tawny.owl/recent-axiom-list))))

(defn- subclass-options
  "Handles disjoint and covering axioms on subclasses."
  [options superclass subclasses]
  (let [optset (into #{} options)]
    (when (and
           (contains? optset :disjoint)
           ;; set them disjoint if there is more than one. if there is only one
           ;; then it would be illegal OWL2. this macro then just shields the body
           ;; from any other as-disjoint statements.
           (< 1 (count tawny.owl/recent-axiom-list)))
      (disjointclasseslist
       recent-axiom-list))
    (when (and
           (contains? optset :cover))
      (add-equivalent
       superclass
       (list (owlor recent-axiom-list))))))

;; hmmm, now how do we do the ontology thing here?
(defmacro declare-classes
  "Declares all the classes given in names.

This is mostly useful for forward declarations, but the classes declared will
have any default frames or disjoints if `as-disjoints' or
`with-default-frames' or equivalent macros are in use.

See `defclassn' to define many classes with frames.
"
  [& names]
  `(do ~@(map
          (fn [x#]
            `(defclass ~x#))
          names)))

(defmacro defclassn
  "Defines many classes at once.

Each class and associated frames should be supplied as a vector.

See `declare-classes' where frames (or just default frames) are not needed.
"
  [& classes]
  `(do ~@(map
          (fn [x#]
            `(defclass ~@x#)) classes)))

;; predicates
(defontfn direct-superclasses
  "Returns the direct superclasses of name.
Name can be either a class or a string name. Returns a list of class
expressions."
  [o name]
  (let [clz (ensure-class name)]
    ;; general Class expressions return empty
    (if (instance? OWLClass clz)
      (.getSuperClasses clz
                        o)
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

(defontfn superclasses [o class]
  "Return all subclasses of class"
  (superclasses-1 o (direct-superclasses o class)))

(defontfn superclass?
  "Returns true is name has superclass as a superclass"
  [o name superclass]
  (let [namecls (ensure-class name)
        superclasscls (ensure-class superclass)]
    (some #(.equals superclasscls %) (superclasses o name))))

(defontfn direct-subclasses
  "Returns the direct subclasses of name."
  [o name]
  (let [clz (ensure-class name)]
    (if (instance? OWLClass clz)
      (.getSubClasses (ensure-class name)
                      o)
      ())))

(declare subclasses)
(defontfn subclass?
  "Returns true if name has subclass as a subclass"
  [o name subclass]
  (let [namecls (ensure-class name)
        subclasscls (ensure-class subclass)]
    (some #(.equals subclasscls %) (subclasses o name))))

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

(defontfn subclasses [o class]
  "Return all subclasses of class"
  (subclasses-1 o (direct-subclasses o class)))

(defontfn disjoint?
  "Returns t iff classes are asserted to be disjoint."
  [o a b]
  (contains?
   (.getDisjointClasses a o)
   b))


(defontfn equivalent?
  "Returns t iff classes are asserted to be equivalent."
  [o a b]
  (contains?
   (.getEquivalentClasses a o)
   b))


;; some test useful macros

;; currently doesn't support an ontology argument
;; modified from with-open
(defmacro with-probe-entities
  "Evaluate the body with a number of entities defined. Then
delete these entities from the ontology"
  [bindings & body]
  (when-not (vector? bindings)
    (IllegalArgumentException. "with-probe-entities requires a vector"))
  (when-not (even? (count bindings))
    (IllegalArgumentException.
     "with-probe-entities requires an even number of forms in binding vector"))
  (cond
   (= (count bindings) 0)
   `(do ~@body)
   (symbol? (bindings 0))
   `(let ~(subvec bindings 0 2)
      (with-probe-entities
        ~(subvec bindings 2)
        ;; try block just so we can use finally
        (try
          ~@body
          (finally
            (tawny.owl/remove-entity ~(bindings 0))))))
   :else
   (throw (IllegalArgumentException.
           "with-probe-entities only allows Symbols in bindings"))))


(defmacro with-probe-axioms
  "Evaluate the body with a number of axioms. Then
delete these axioms from the ontology.

This is mostly useful for test cases. Axioms can be added, consistency
or inconsistency can be checked then removed, leaving the ontology
effectively unchanged."
  [bindings & body]
  (when-not (vector? bindings)
    (IllegalArgumentException. "with-probe-axioms requires a vector"))
  (when-not (even? (count bindings))
    (IllegalArgumentException.
     "with-probe-axioms requires an even number of forms in binding vector"))
  (cond
   (= (count bindings) 0)
   `(do ~@body)
   (symbol? (bindings 0))
   `(let ~(subvec bindings 0 2)
      (with-probe-axioms
        ~(subvec bindings 2)
        ;; try block just so we can use finally
        (try
          ~@body
          (finally
            (tawny.owl/remove-axiom ~(bindings 0))))))
   :else
   (throw (IllegalArgumentException.
           "with-probe-axioms only allows Symbols in bindings"))))

(defn owlthing
  "Object representing OWL thing."
  []
  (.getOWLThing ontology-data-factory))

(defn owlnothing
  "Object representing OWL nothing."
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

(defn- prefix-symbol [prefix sym]
  "Add a prefix to a symbol and return a new symbol."
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
   x
   ))

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
entity. See 'owlclass' or 'objectproperty' for more details.

This is useful for two main reasons. First, to build class definitions in two
places and add frames in both of these places. For simple forward declaration
'declare-classes' is better. The second is where the same class needs to
appear in two ontologies, but with more axioms in the second. This can enable,
for example, building two interlocking ontologies with different OWL profiles.
"
  (fn [owlentity & frames] (class owlentity)))

(defmethod refine OWLClass [& args]
  (apply owlclass args))

(defmethod refine OWLObjectProperty [& args]
  (apply objectproperty args))

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
