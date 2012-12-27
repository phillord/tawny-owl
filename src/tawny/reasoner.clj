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

(ns tawny.reasoner
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
    (org.semanticweb.elk.owlapi ElkReasonerFactory)
    (org.semanticweb.owlapi.reasoner SimpleConfiguration)
    (org.semanticweb.HermiT Reasoner)))

(def vreasoner-factory
  (ref ()))

(def reasoner-list
  (ref ()))

(defn reasoner-factory
  ([]
     @vreasoner-factory)
  ([reasoner]
     (dosync
      ;; blitz the reasoner list
      (ref-set reasoner-list ())
      ;; create a new reasoner
      (ref-set vreasoner-factory
               (reasoner
                {:elk (ElkReasonerFactory.)
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

;; set up the default!
(def
  ^{:dynamic true}
  *reasoner-progress-monitor*
  reasoner-progress-monitor-gui)

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
(defn reasoner []
  (let [reas (reasoner-for-ontology (owl/get-current-jontology))]
    (if reas
      reas
      (let [reas
            (.createReasoner
             (reasoner-factory)
             (owl/get-current-jontology)
             (SimpleConfiguration.
              (*reasoner-progress-monitor*)))]
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

