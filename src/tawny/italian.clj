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
  #'tawny.italian/oggetto-proprietà)

(def individuale
  (italiano o/individual))

;; ontology ontologia
;; def (as is "define”) definizione
;; class classe
;; object oggetto
;; annotation annotazione
;; property proprieta’
;; data dati
;; some alcuni
;; only solo
;; and e
;; or o
;; not no
;; inverse inversa (eg proprieta’ inversa)
;; label etichetta
;; comment commento
;; is defined by (e’ definito da)
;; see also (vedi anche)
;; backward compatible with (compatibile con) — perhaps “retro-compatibile"
;; incompatible with (incompatibile con)
;; version info (versione)
;; deprecated deprecato (!!)
;; super (as in superclass) super (it’s Latin!) o super-classe
;; sub sotto (as in sotto-classe)
;; import importate (eg ontologie importate)
;; prefix prefisso
;; suffix suffisso
;; disjoint disgiunta (eg classi disgiunte)
;; equivalent equivalente
;; transitive transitiva 
;; functional funzionale
;; inverse functional (funzionale inversa)
;; asymmetric asimmetrica
;; symmetric simmetrica
;; irreflexive irriflessiva
;; reflexive riflessiva
;; subchain ?? not sure
;; characteristic caratteristica
;; domain dominio
;; range co-dominio
;; exactly esattamente
;; at least almeno
;; at most al piu’
;; has value ha valore
;; fact fatto
;; is e’
;; type tipo
;; different diverso/a
;; axiom assioma
;; probe ??
;; refine rifinire / rifinisce
;; span ??
