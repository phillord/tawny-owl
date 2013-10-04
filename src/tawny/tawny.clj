(ns
    ^{:doc "An ontology for annotations of tawny"
      :author "Phillip Lord"} tawny.tawny
  (:refer-clojure :only [var-get])
  (:use [tawny.owl :as o]))


;; The sensible thing would be to build this ontology in tawny and then load
;; it directly, but this causes bootstrap problems. So, the ontology is build
;; "off-line", then loaded from the OWL file into tawny.owl at load time.

(defontology tawny
  :iri (var-get #'o/tawny-base-url)
  ;; We need to nobble the annotation for this ontology.
  :noname true)

(defaproperty name)


(save-ontology tawny "resources/tawny.owl" :owl)
