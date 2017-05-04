(ns
    ^{:doc "Italian translation of Tawny OWL. Currently incomplete"
      :author "Phillip Lord"}
    tawny.italian
  (:require
   [tawny.owl :as o]
   [tawny.polyglot :as p]))

(def italiano-traduzione-mappa
  {
   :ontology :ontology

   ;; ontologia maneggiatore
   :noname :nonnome
   :iri-gen :iri-gen
   :prefix :prefisso
   :name :nome
   :seealso :vedianche
   :comment :commento
   :versioninfo :versione
   :annotation :annotazione
   :import :importate

   ;; annotazione maneggiatore
   ;; :super lo stesso
   :label :etichetta

   ;; oggetto proprietà
   :domain :dominio
   :range :co-dominio
   :inverse :inversa
   :characteristic :caratteristica
   :subchain :sottocatena
   :disjoint :disgiunta
   :equivalent :equivalente

   ;; classe
   :haskey :hachiave

   ;; individuale
   :type :tipo
   :fact :fatto
   :same :stesso
   :different :diverso})

(defn italiano [f]
  (p/polyglot-trans f italiano-traduzione-mappa))

(def ontologia
  (italiano o/ontology))

(defmacro defontologia [name & argi]
  (o/ontology-def-f name argi))

(def annotazione-proprietà
  (italiano o/annotation-property))

(o/defentity defaproprietà
  ""
  'tawny.italian/annotazione-proprietà
  :ontologia)

(def oggetto-proprietà
  (italiano o/object-property))

(o/defentity defoproprietà
  ""
  #'tawny.italian/oggetto-proprietà
  :ontologia)

(def individuale
  (italiano o/individual))

(o/defentity defindividuale "" #'tawny.italian/individuale
  :ontologia)

(def classe (italiano o/owl-class))

(o/defentity defclasse "" #'tawny.italian/classe
  :ontologia)

(def datiproprietà (italiano o/datatype-property))
(o/defentity defdproprietà "" #'tawny.italian/datiproprietà
  :ontologia)

(def datitipo (italiano o/datatype))
(o/defentity defdatitipo "" #'tawny.italian/datitipo
  :ontologia)

(def alcuni #'o/owl-some)
(def solo #'o/only)
(def e #'o/owl-and)
(def o #'o/owl-or)
(def no #'o/owl-not)
(def inversa #'o/inverse)
(def etichetta #'o/label)
(def commento #'o/owl-comment)
(def è-definito-da #'o/is-defined-by)
(def vedi-anche #'o/see-also)
(def retro-compatibile #'o/backward-compatible-with)
(def versione #'o/version-info)
(def deprecato #'o/deprecated)
(def come-disgiunta #'o/as-disjoint)
(def almeno #'o/at-least)
(def al-più #'o/at-most)
(def ha-valore #'o/has-value)
(def è #'o/is)
(def refinire #'o/refine)
(def campata #'o/span)
