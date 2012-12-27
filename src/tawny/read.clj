(ns tawny.read
  (:require [tawny.owl])
  (:refer-clojure :exclude [read])
  (:import
   (java.io File)
   (java.net URL)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.semanticweb.owlapi.model IRI OWLNamedObject OWLOntologyID)))




(defn- default-filter [e]
  (and (instance? OWLNamedObject e)
       (= (tawny.owl/get-current-iri)
          (.getStart (.getIRI e)))))


(defn- default-transform [e]
  (.. e (getIRI) (getFragment)))


(defn- intern-entity [e transform]
  (when (instance? OWLNamedObject e)
    (let [name (transform e)]
      (intern *ns* (symbol name) e))))


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
        
        ontology
        (tawny.owl/generate-ontology iri prefix owlontology)
        ]

    ;; this is the ontology for the namespace so stuff it place
    (tawny.owl/ontology-to-namespace ontology)

    ;;
    (doall
     (map
      (fn [x]
        ;; grab each entity, put classes, object properties and so forth into current system.

        ;; define a multi method keyed on (part of a) URI which does the work. 
        (intern-entity x
                       (or transform default-transform))
        )

      ;; filter this so that it only puts stuff with the given IRI prefix
      (clojure.core/filter (or filter default-filter)
                           (.getSignature owlontology))))

    ontology))



(defmacro defread [symbol & rest]
  `(do
    (def ~symbol
      (tawny.read/read ~@rest))))
