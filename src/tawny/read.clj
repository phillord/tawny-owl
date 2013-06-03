(ns tawny.read
  (:require [tawny.owl]
            [clojure.string :only replace])
  (:refer-clojure :exclude [read])
  (:import
   (java.io File)
   (java.net URL)
   (tawny.util CallbackString)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.semanticweb.owlapi.model IRI OWLNamedObject OWLOntologyID)))


(defn- default-filter [e]
  (and (instance? OWLNamedObject e)
       (= (tawny.owl/get-current-iri)
          (.getStart (.getIRI e)))))


(defn- default-transform [e]
  (.. e (getIRI) (getFragment)))

(defn iri-starts-with-filter 
  "Checks e to see if it is an OWLNamedObject and has an IRI starting with
starts-with. Use this partially applied with a filter for 'read'."
  [starts-with e]
  (and (instance? OWLNamedObject e)
       (.startsWith
        (.toString (.getIRI e))
        starts-with)))

(defn filter-for-labels 
  "Filter annotations on an entity for labels"
  [e]
  (filter 
   #(some-> % 
        (.getProperty)
        (.isLabel))
   (.getAnnotations e (tawny.owl/get-current-ontology))))

(defn label-transform 
  "Get text from label annotation"
  [e]
  (some-> (filter-for-labels e)
      (first)
      (.getValue)
      (.getLiteral)))

(defn noisy-nil-label-transform 
 "Check for empty labels noisily"
 [e]
 (let [trans (label-transform e)]
    (when (nil? trans)
      (println "Unable to generate transform for:" e))
    trans
    ))

(defn exception-nil-label-transform 
 "Check for empty labels noisily"
 [e]
  (let [trans (label-transform e)]
    (when (nil? trans)
      (throw (IllegalArgumentException. (str "Unable to generate transform for:" e))))
    trans
    ))

(defn fragment-transform
  "Create an entity name from the IRI fragment"
  [e]
  (-> e
      (.getIRI)
      (.getFragment)))

(defn stop-characters-transform 
  "Takes a string and treats characters not legal in a 
Clojure symbol. Use this composed with a entity transform function"
  [s]
  (clojure.string/replace s #"[ /]" "_"))

(defn intern-entity 
  ([e]
     (intern-entity e fragment-transform))
  ([e transform]
     (try
       (when (instance? OWLNamedObject e)
         (let [name
               (stop-characters-transform (transform e))]
           (tawny.owl/intern-owl *ns* (symbol name) e {})))
       (catch IllegalArgumentException i 
         (print "Broken Intern on:" e)
         (throw i)))))


(defn read [& rest]
  (let [{:keys [location iri file prefix filter transform version-iri]} rest
        
        jiri (IRI/create iri)
        viri (if version-iri
               (IRI/create version-iri))

        ontologyid
        (OWLOntologyID. jiri viri)
        
        owlontology
        (do
          (tawny.owl/remove-ontology-maybe ontologyid)
          (.loadOntologyFromOntologyDocument
           tawny.owl/owl-ontology-manager
           location))
        ]

    (when prefix
      (let [format (.getOntologyFormat tawny.owl/owl-ontology-manager owlontology)]
        (if (.isPrefixOWLOntologyFormat format)
          (.setPrefix format prefix (.toString iri))
          (throw (IllegalArgumentException. "Attempt to provide a prefix to an ontology that is not using a prefix format")))))
    
    ;; this is the ontology for the namespace so stuff it place
    (tawny.owl/ontology-to-namespace owlontology)

    ;;
    (doall
     (map
      (fn [x]
        ;; grab each entity, put classes, object properties and so forth into
        ;; current system.
        (intern-entity x
                       (or transform default-transform))
        )

      ;; filter this so that it only puts stuff with the given IRI prefix
      (doall 
       (clojure.core/filter (or filter default-filter)
                                   (.getSignature owlontology)))))

    owlontology))


(defn iri-create
  "A convienience method for creating IRIs.

Most namespaces that call use 'defread' will need to create an IRI. 
This convienience method avoids the need for importing and depending 
directly on the OWL API."
  [iri]
  (IRI/create iri))

(defmacro defread [symbol & rest]
  `(do
    (def ~symbol
      (tawny.read/read ~@rest))))
