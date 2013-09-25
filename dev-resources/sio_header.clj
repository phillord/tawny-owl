(ns sio-header
  (:use [tawny.owl]))

;; don't use defontology because it puts a name annotation in
;; which confuses all my counts
(def sio-rendered
  (ontology))
(ontology-to-namespace (find-ns 'sio-header) sio-rendered)

(load "sio_rendered")
