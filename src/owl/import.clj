(ns owl.import
  (:require [owl.owl])
  (:import
   (java.io File)
   (java.net URL)
   (org.semanticweb.owlapi.apibinding OWLManager)
   (org.semanticweb.owlapi.model IRI OWLNamedObject)))




(defn- default-filter [e]
  (and (instance? OWLNamedObject e)
       (= (owl.owl/get-current-iri)
          (.getStart (.getIRI e)))))


(defn- default-transform [e]
  (.. e (getIRI) (getFragment)))


(defn- intern-entity [e transform]
  (when (instance? OWLNamedObject e)
    (let [name (transform e)]
      (intern *ns* (symbol name) e))))


(defn owlimport [location & rest]
  (let [{:keys [iri file prefix filter transform]} rest
        manager (OWLManager/createOWLOntologyManager)
        
        owlontology
        (.loadOntologyFromOntologyDocument
         manager
         location)

        ontology
        (owl.owl/generate-ontology iri prefix manager owlontology)
        ]

    ;; this is the ontology for the namespace so stuff it place
    (owl.owl/ontology-to-namespace ontology)

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
              (.getSignature owlontology))))))




