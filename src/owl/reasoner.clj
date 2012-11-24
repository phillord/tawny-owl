;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(ns owl.reasoner
  (:require [owl.owl :as owl])
  (import
   (javax.swing BoxLayout
    JFrame JPanel JProgressBar JLabel WindowConstants)
   
   (org.semanticweb.owlapi.reasoner
           ConsoleProgressMonitor InferenceType Node
           NodeSet OWLReasoner OWLReasonerConfiguration
           OWLReasonerFactory SimpleConfiguration)
          
          (org.semanticweb.elk.owlapi ElkReasonerFactory)
          (org.semanticweb.HermiT Reasoner)
          (org.semanticweb.owlapi.reasoner.structural
           StructuralReasonerFactory StructuralReasoner)))

;; need to do this better
(declare vreasoner-factory)

(defn reasoner-factory
  ([]
     vreasoner-factory)
  ([reasoner]
     (def vreasoner-factory
       (reasoner
        {:elk (ElkReasonerFactory.)
         :hermit (org.semanticweb.HermiT.Reasoner$ReasonerFactory.)
         }

        ))))

;; set default
;; (reasoner-factory :elk)


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
        (println "Reasoner task busy");; stuff
        )
      (reasonerTaskProgressChanged [val max]
        (println "Reasoner task changed" val ":" max)
        (doto progressbar
          (.setIndeterminate false)
          (.setMaximum max)
          (.setValue val)))
      (reasonerTaskStarted [name]
        (println "reasoner task started" name)
        (.setText label name)
        (doto frame
          (.pack)
          (.setVisible true)))
      (reasonerTaskStopped []
        (.setVisible frame false)
        (println "reasoner task stopped")))))


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


;; we need to cache these 'cause reasoners listen to changes could just use
;; memoized function taking jontology as param Probably need to write a new
;; ProgressMonitor to communicate with emacs.
(defn reasoner []
  (.createReasoner (reasoner-factory)
                   (owl/get-current-jontology)
                   (SimpleConfiguration.
                    (reasoner-progress-monitor-gui))))

(defn consistent?
  "Returns true if the ontology is consistent.

This method can throw an InconsistentOntologyException
"
  []
  ;; (.precomputeInferences (reasoner)
  ;;                        ;; vars args ugliness
  ;;                        (into-array InferenceType
  ;;                                    (list InferenceTyp
  ;;                                           e/CLASS_HIERARCHY)))
  (do
    (.isConsistent (reasoner))))

(defn unsatisfiable
  "Returns all unsatisfiable classes from the current ontology

Throws an org.semanticweb.owlapi.reasoner.InconsistentOntologyException if the
ontology is inconsistent"
  []
  ;; bottom is always inconsistent!
  (.getEntitiesMinusBottom
   (.getUnsatisfiableClasses
    (reasoner))))

(defn coherent?
  "Returns true if the ontology is coherent"
  []
  ;; actually implement this -- satisfiable > 0
  (zero? (count (unsatisfiable))))

