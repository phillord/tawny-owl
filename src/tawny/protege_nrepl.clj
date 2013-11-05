;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, Phillip Lord, Newcastle University
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
    ^{:doc "Supports Protege NREPL plugin integration"
      :author "Phillip Lord"}
  tawny.protege-nrepl
  ;; neither of these requires are in the project.clj dependency list, as we
  ;; expect to use this only in the context of the protege nrepl plugin, which
  ;; should already have them
  (:require
   [tawny.owl]
   [tawny.util]
   [cemerick.pomegranate]
   [protege.model]))

;; and here is a test to see that it's all good, against the current version.
(alter-var-root
 #'tawny.owl/owl-data-factory
 (constantly
  (fn [] (.getOWLDataFactory protege.model/*owl-model-manager*))))

(alter-var-root
 #'tawny.owl/owl-ontology-manager
 (constantly
  (fn [] (.getOWLOntologyManager protege.model/*owl-model-manager*))))

(defn protege-remove-ontology-maybe
  "Modified version of remove-ontology-maybe which does exactly the same thing
as the normal one, but calls the relevant protege method instead of the OWL
API which leaves the GUI in an inconsistent state."
  [ontologyid]
  (when (.contains (tawny.owl/owl-ontology-manager) ontologyid)
    (let [o (.getOntology (tawny.owl/owl-ontology-manager) ontologyid)]
      (.removeOntology
       ;; this is the different bit -- remove-ontology-maybe removes from the
       ;; model manager, but this tends to break things!
       protege.model/*owl-model-manager* o)
      ;; remove the ontology options
      (dosync
       (swap! tawny.owl/ontology-options-atom
              dissoc o))
      ;; remove the ontology from the namespace map
      (#'tawny.owl/remove-ontology-from-namespace-map o)
      (tawny.util/run-hook tawny.owl/remove-ontology-hook o)
      o)))

(alter-var-root
 #'tawny.owl/remove-ontology-maybe
 (constantly protege-remove-ontology-maybe))

(defn make-last-ontology-active [ontology & _]
  (protege.model/active-ontology ontology))

(alter-var-root
 #'tawny.owl/ontology-handlers
 (fn [handlers]
   (merge handlers {::creation-hook make-last-ontology-active})))

(defn repl-client-connected [editorkit]
  (some-> @protege.nrepl/servers
          (get editorkit)
          (get :open-transports)
          (deref)
          (count)
          (zero?)
          (not)))

(defn swap-dirty-close-warning []
  (let [editorkit protege.model/*owl-editor-kit*
        frame
        (-> (org.protege.editor.core.ProtegeManager/getInstance)
            (.getEditorKitManager)
            (.getWorkspaceManager)
            (.getFrame protege.model/*owl-work-space*))
        listener
        (first
         ;; the one we want is anonymous
         (filter
          #(=
            org.protege.editor.core.ui.workspace.WorkspaceManager
            (.getEnclosingClass (class %)))
          ;; get all the windows listeners
          (-> frame
              (.getWindowListeners))))]
    (.removeWindowListener frame listener)
    (.addWindowListener
     frame
     (proxy [java.awt.event.WindowAdapter] []
       (windowClosing [event]
         (when
             (and
              (repl-client-connected editorkit)
              (= (javax.swing.JOptionPane/showConfirmDialog
                  frame
                  "An NREPL client is still connected to this workspace. Close anyway?"
                  "Nrepl Connected"
                  javax.swing.JOptionPane/YES_NO_OPTION
                  javax.swing.JOptionPane/WARNING_MESSAGE)
                 javax.swing.JOptionPane/YES_OPTION))
           (-> (org.protege.editor.core.ProtegeManager/getInstance)
               (.disposeOfEditorKit editorkit))
           (.removeWindowListener frame this)
           (.dispose frame)))))))

(defn kill-import-warning []
  (.setMissingImportHandler
   protege.model/*owl-model-manager*
   (proxy [org.protege.editor.owl.model.MissingImportHandler] []
     (getDocumentIRI [iri]))))

(defn selected-object-maybe [entity]
  (when-let [workspace protege.model/*owl-work-space*]
    (javax.swing.SwingUtilities/invokeLater
     #(protege.model/selected-object workspace entity))))

(defn active-ontology-maybe [ontology]
  (when protege.model/*owl-work-space*
    (protege.model/active-ontology ontology)))

(defn display-maybe [ns entity]
  (.println System/out (str "display:" entity))
  (when-let
      [entity
       (first
        (filter identity
                (map
                 #(tawny.owl/entity-for-string % entity)
                 (.getOntologies (tawny.owl/owl-ontology-manager)))))]
      (selected-object-maybe entity)))
