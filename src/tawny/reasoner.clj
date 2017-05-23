;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University
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
(ns
    ^{:doc "Interaction with external reasoners"
      :author "Phillip Lord"}
  tawny.reasoner
  (:use [tawny.owl :only [defno]])
  (:require [tawny.owl :as owl]
            [tawny.util :as util])
  (:import
   (java.lang.ref WeakReference)
   (java.util WeakHashMap)
   (javax.swing
      BoxLayout
      JFrame
      JLabel
      JPanel
      JProgressBar
      WindowConstants)
   (java.awt GraphicsEnvironment)
   (org.semanticweb.owlapi.model OWLOntology)
   (org.semanticweb.owlapi.reasoner
    OWLReasoner OWLReasonerFactory
    NodeSet)
   (org.semanticweb.elk.owlapi ElkReasonerFactory)
   (org.apache.log4j
    Level
    Logger)
   (org.semanticweb.owlapi.reasoner SimpleConfiguration)
   (org.semanticweb.HermiT Reasoner)))

(defn- reasoner-factory-1 [reasoner-keyword]
  (reasoner-keyword
   {:elk
    (do
      ;; ELK is noisy, so shut it up
      (-> (Logger/getLogger "org.semanticweb.elk")
          (.setLevel Level/ERROR));
      (ElkReasonerFactory.))
    :hermit (org.semanticweb.HermiT.Reasoner$ReasonerFactory.)
    :jfact (uk.ac.manchester.cs.jfact.JFactFactory.)
    :nil nil}))

;; defonce semantics because a new reasoner factory should cause us
;; to drop all existing reasoners.
(defonce
  ^{:doc "The current reasoner factory."
    :private true}
  vreasoner-factory
  (ref (reasoner-factory-1 :jfact)))

;; defonce semantics because reasoners do not necessarily clear up nicely
;; even after GC.
(defonce
  ^{:doc "A list of the reasoners currently in use"
    :private true}
  reasoner-list
  (ref ()))

(defn ^OWLReasonerFactory reasoner-factory
  "Return or set the reasoner factory. The reasoner must be either
  :hermit, :elk or :jfact. It can also be :nil, which will leave no reasoner
  factory set."
  ([]
     (when (nil? @vreasoner-factory)
       (throw (IllegalStateException. "No reasoner has been chosen")))
     @vreasoner-factory)
  ([reasoner-keyword]
     (dosync
      ;; blitz the reasoners
      ;; (doseq [^OWLReasoner r @reasoner-list]
      ;;   (when-not (instance? uk.ac.manchester.cs.jfact.JFactReasoner r)
      ;; (.dispose r)))
      ;; blitz the reasoner list
      (ref-set reasoner-list ())
      ;; create a new reasoner
      (ref-set vreasoner-factory
               (reasoner-factory-1 reasoner-keyword)))))

(defn reasoner-progress-monitor-gui
  "Return a new graphical progress monitor."
  []
  (let [progressbar (JProgressBar.)
        frame (JFrame. "Reasoner Progress")
        content (JPanel.)
        label (JLabel.)
        ]
    (doto frame
      (.setDefaultCloseOperation WindowConstants/HIDE_ON_CLOSE)
      (.add content))
    (doto content
      (.setLayout (BoxLayout. content BoxLayout/Y_AXIS))
      (.add progressbar)
      (.add label))
    (.setIndeterminate progressbar true)
    (proxy [org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor] []
      (reasonerTaskBusy[]
        ;;(println "Reasoner task busy");; stuff
        )
      (reasonerTaskProgressChanged [val max]
        (doto progressbar
          (.setIndeterminate false)
          (.setMaximum max)
          (.setValue val)))
      (reasonerTaskStarted [name]
        (.setText label name)
        (doto frame
          (.pack)
          (.setVisible true)))
      (reasonerTaskStopped []
        (.setVisible frame false)))))


(defn reasoner-progress-monitor-text
  "Return a new text based progress monitor."
  []
  (proxy [org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor] []
    (reasonerTaskBusy[]
      (println "Reasoner task busy");; stuff
      )
    (reasonerTaskProgressChanged [val max]
      (println "Reasoner task changed" val ":" max)
      )
    (reasonerTaskStarted [name]
      (println "reasoner task started" name))
    (reasonerTaskStopped []
      (println "reasoner task stopped"))))


(defn reasoner-progress-monitor-silent
  "Returns a new progress monitor which is silent."
  []
  (proxy [org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor] []
    (reasonerTaskBusy[]
      )
    (reasonerTaskProgressChanged [_ _]
      )
    (reasonerTaskStarted [_]
      )
    (reasonerTaskStopped []
      )))

(defn reasoner-progress-monitor-gui-maybe
  "Return a gui monitor unless we are headless."
  []
  (if (GraphicsEnvironment/isHeadless)
    (reasoner-progress-monitor-text)
    (reasoner-progress-monitor-gui)))

;; set up the default!
(def
  ^{:dynamic true
    :doc "The current progress monitor to use."}
  *reasoner-progress-monitor*
  (atom reasoner-progress-monitor-gui-maybe))

(defn reasoner-silent
  "Shut the reasoner up"
  []
  (reset! *reasoner-progress-monitor* reasoner-progress-monitor-silent))

(defn ^OWLReasoner reasoner-for-ontology
  "Return an reasoner for the given ontology if it exists. Normally
the reasoner function is better to use."
  [ontology]
  (first
   (filter
    #(= (System/identityHashCode
         ontology)
        (System/identityHashCode
         (.getRootOntology ^OWLReasoner %)))
    @reasoner-list)))

;; we need to cache these 'cause reasoners listen to changes could just use
;; memoized function taking jontology as param Probably need to write a new
;; ProgressMonitor to communicate with emacs.
(defno ^OWLReasoner reasoner
  "Returns a reasoner for the given ontology, creating a new one if necessary
from the current reasoner-factory."
  [ontology]
  (let [reas (reasoner-for-ontology ontology)]
    (if reas
      reas
      (let [reas
            (.createNonBufferingReasoner
             (reasoner-factory)
             ontology
             (SimpleConfiguration.
              ^org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor
              ((deref *reasoner-progress-monitor*))))]
        (dosync
         (ref-set reasoner-list (conj @reasoner-list reas)))
        reas))))


(do
  ;; define the hook function
  (defn discard-reasoner
    "Explicitly discard the reasoner for a given ontology. This should
happen automatically."
    [ontology]
    (dosync
     (let [reasoner (reasoner-for-ontology ontology)]
       (when-not (nil? reasoner)
         (ref-set reasoner-list
                  (remove #{reasoner} @reasoner-list))
         (when-not
             (or
              ;; JFact crashes at the moment because I've already removed the
              ;; ontology from the manager
              (instance? uk.ac.manchester.cs.jfact.JFactReasoner reasoner)
              ;; some does HermiT -- which is the reason I put this code in
              ;; place in the first instance.
              (instance? org.semanticweb.HermiT.Reasoner reasoner))
           (.dispose reasoner))))))

  ;; add in do, so that we can't do one without the other
  (util/add-hook owl/remove-ontology-hook
                 discard-reasoner))

(defno consistent?
  "Returns true if the ontology is consistent.

This method can throw an InconsistentOntologyException
"
  [ontology]
  ;; (.precomputeInferences (reasoner)
  ;;                        ;; vars args ugliness
  ;;                        (into-array InferenceType
  ;;                                    (list InferenceTyp
  ;;                                           e/CLASS_HIERARCHY)))
  (.isConsistent (reasoner ontology)))

(defno unsatisfiable
  "Returns all unsatisfiable classes from the current ontology

Throws an org.semanticweb.owlapi.reasoner.InconsistentOntologyException if the
ontology is inconsistent"
  [ontology]
  ;; bottom is always inconsistent!
  (.getEntitiesMinusBottom
   (.getUnsatisfiableClasses
    (reasoner ontology))))

(defno coherent?
  "Returns true if the ontology is coherent"
  [ontology]
  ;; actually implement this -- satisfiable > 0
  (zero? (count (unsatisfiable ontology))))

(defn entities
  "Return all entities for a nodeset."
  [^NodeSet nodeset]
  (set (.getFlattened nodeset)))

(defn no-top-bottom
  "Delete top and bottom from a collection."
 [coll]
  (set
   (filter #(not
             (or (= (owl/owl-thing) %1)
                 (= (owl/owl-nothing) %1)))
           coll)))

(defno isuperclasses
  "Return all superclasses in ontology for name. Returns a (clojure)
set and does not return top or bottom."
  [ontology name]
  (no-top-bottom
   (entities
    (.getSuperClasses (reasoner ontology)
                      (#'tawny.owl/ensure-class name)
                      false))))

;; move this to using isuperclasses
(defno isuperclass?
  "Returns true if name has superclass as a strict superclass."
  [ontology name superclass]
  (let [superclasses
        (isuperclasses ontology name)]
    (if (some #{superclass} superclasses)
      true false)))

(defno isubclasses
  "Returns all infered subclasses of name in ontology o.
Returns a clojure set, and does not return top or bottom."
  [o name]
  (no-top-bottom
   (entities
    (.getSubClasses (reasoner o)
                    (#'tawny.owl/ensure-class name)
                    false))))

(defno isubclass?
  "Returns true if name has subclass as a strict subclass"
  [ontology name subclass]
  (let [subclasses
        (isubclasses ontology name)]
    (if (some #{subclass} subclasses)
      true false)))

(defno iequivalent-classes
  "Returns equivalent classes to name in ontology. Returns a set which
may include top or bottom."
  [ontology name]
  (.getEntities
   (.getEquivalentClasses (reasoner ontology)
                          (#'tawny.owl/ensure-class name))))

(defno iequivalent-class?
  "Returns true if name and equiv are equivalent in ontology."
  [ontology name equiv]
  (let [equivs
        (iequivalent-classes ontology name)]
    (if (some #{equiv} equivs)
      true false)))

(defno instances
  "Returns all instances of class in ontology."
  [ontology class]
  (entities
   (.getInstances (reasoner ontology)
                  class false)))
