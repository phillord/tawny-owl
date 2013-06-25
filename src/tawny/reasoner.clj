;; Copyright (C) 2011, Newcastle University

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


(ns tawny.reasoner
  (:use [tawny.owl :only [defontfn]])
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
    (org.semanticweb.elk.owlapi ElkReasonerFactory)
    (org.apache.log4j 
     Level
     Logger)
    (org.semanticweb.owlapi.reasoner SimpleConfiguration)
    (org.semanticweb.HermiT Reasoner)))

(def vreasoner-factory
  (ref nil))

(def reasoner-list
  (ref ()))

(defn reasoner-factory
  ([]
     (when (nil? @vreasoner-factory)
       (throw (IllegalStateException. "No reasoner has been chosen")))
     @vreasoner-factory)
  ([reasoner]
     (dosync
      ;; blitz the reasoners
      (doseq [r @reasoner-list]
        (.dispose r))
      ;; blitz the reasoner list
      (ref-set reasoner-list ())
      ;; create a new reasoner
      (ref-set vreasoner-factory
               (reasoner
                {:elk 
                 (do
                   ;; ELK is noisy, so shut it up
                   (-> (Logger/getLogger "org.semanticweb.elk")
                       (.setLevel Level/ERROR));
                   (ElkReasonerFactory.))
                 :hermit (org.semanticweb.HermiT.Reasoner$ReasonerFactory.)
                 }
                )))))


(def
  ^{:private true
    :dynamic true}
  *reasoner-start-time*
  )

(defn reasoner-progress-monitor-gui []
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


(defn reasoner-progress-monitor-text []
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


(defn reasoner-progress-monitor-silent[]
  (proxy [org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor] []
    (reasonerTaskBusy[]
      )
    (reasonerTaskProgressChanged [val max]
      )
    (reasonerTaskStarted [name]
      )
    (reasonerTaskStopped []
      )))

(defn reasoner-progress-monitor-gui-maybe []
  (if (GraphicsEnvironment/isHeadless)
    (reasoner-progress-monitor-text)
    (reasoner-progress-monitor-gui)))

;; set up the default!
(def
  ^{:dynamic true}
  *reasoner-progress-monitor*
  (atom reasoner-progress-monitor-gui-maybe))

(defn reasoner-for-ontology [ontology]
  (first
   (filter
    #(= (System/identityHashCode
         ontology)
        (System/identityHashCode
         (.getRootOntology %)))
    @reasoner-list)))

;; we need to cache these 'cause reasoners listen to changes could just use
;; memoized function taking jontology as param Probably need to write a new
;; ProgressMonitor to communicate with emacs.
(defontfn reasoner [ontology]
  (let [reas (reasoner-for-ontology ontology)]
    (if reas
      reas
      (let [reas
            (.createNonBufferingReasoner
             (reasoner-factory)
             (owl/get-current-ontology)
             (SimpleConfiguration.
              ((deref *reasoner-progress-monitor*))))]
        (dosync
         (ref-set reasoner-list (conj @reasoner-list reas)))
        reas))))


(do
  ;; define the hook function
  (defn discard-reasoner [ontology]
    (dosync
     (let [reasoner (reasoner-for-ontology ontology)]
       (when-not (nil? reasoner)
         (ref-set reasoner-list
                  (remove #{reasoner} @reasoner-list))
         (.dispose reasoner)))))
  
  ;; add in do, so that we can't do one without the other
  (util/add-hook owl/remove-ontology-hook
                 discard-reasoner))


(defontfn consistent?
  "Returns true if the ontology is consistent.

This method can throw an InconsistentOntologyException
"
  [ontology]
  ;; (.precomputeInferences (reasoner)
  ;;                        ;; vars args ugliness
  ;;                        (into-array InferenceType
  ;;                                    (list InferenceTyp
  ;;                                           e/CLASS_HIERARCHY)))
  (do
    (.isConsistent (reasoner ontology))))

(defontfn unsatisfiable
  "Returns all unsatisfiable classes from the current ontology

Throws an org.semanticweb.owlapi.reasoner.InconsistentOntologyException if the
ontology is inconsistent"
  [ontology]
  ;; bottom is always inconsistent!
  (.getEntitiesMinusBottom
   (.getUnsatisfiableClasses
    (reasoner ontology))))

(defontfn coherent?
  "Returns true if the ontology is coherent"
  [ontology]
  ;; actually implement this -- satisfiable > 0
  (zero? (count (unsatisfiable ontology))))

(defn- class-in-node-set? [nodeset class]
  (util/in?
   (.getFlattened nodeset) class))

(defn entities [nodeset]
  (into #{} (.getFlattened nodeset)))

(defn no-top-bottom [coll]
  (into #{}
        (filter #(not
                  (or (= (owl/owlthing) %1)
                      (= (owl/owlnothing) %1)))
                coll)))


;; returns an immutable set of Nodes (including NodeSet's I think).
(defontfn isuperclasses [ontology name]
  (no-top-bottom
   (entities)
   (.getSuperClasses (reasoner ontology)
                     (owl/ensure-class name)
                     false)))

;; move this to using isuperclasses
(defontfn isuperclass?
  "Returns true if name has superclass as a strict superclass"
  [ontology name superclass]
  (let [superclasses
        (isuperclasses ontology name)]
    (class-in-node-set? superclasses superclass)))


(defontfn isubclasses [ontology name]
  (.getSubClasses (reasoner ontology)
                  (owl/ensure-class name)
                  false))

(defontfn isubclass?
  "Returns true if name has subclass as a strict subclass"
  [ontology name subclass]
  (let [subclasses
        (isubclasses ontology name)]
    (class-in-node-set? subclasses subclass)))

(defontfn iequivalent-classes [ontology name]
  (no-top-bottom
   (entities
    (.getEquivalentClasses (reasoner ontology)
                           (owl/ensure-class name)))))

(defontfn iequivalent-class? [ontology name equiv]
  (util/in?
   (iequivalent-classes ontology name)
   equiv))

(defontfn instances [ontology class]
  (.getInstances (reasoner ontology)
                class false))
