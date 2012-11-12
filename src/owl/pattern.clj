
(ns owl.pattern
  (:require [owl.owl :as o]))


(defmacro value-partition [& definition]
  (let [documentation#
        (if (string? (first definition))
          (list :comment (first definition))
          ())
        superclass#
        (if (string? (first definition))
          (second definition)
          (first definition))
        values#
        (if (string? (first definition))
          (nth definition 2)
          (second definition))
        propertyname#
        (symbol
         (str "has"
              (name superclass#)))]

    ;; need to think what to do about the doc string. 
    `(do 
       ;; need a forward declaration
       (o/defclass ~superclass#)
       ;; haven't added documentation to defoproperty yet
       (o/defoproperty ~propertyname#
         :characteristics o/functional
         )
       (o/with-default-frames
         [:subclass ~superclass#
          ~@documentation#
          ]
        
         (o/declare-classes ~@values#))

       ;; put in covering axiom once we worked out how to do this. 
       (o/defclass ~superclass#
         ~@documentation#
         )

       (o/add-disjoint-union ~superclass# ~values#))))



;; hmmm
(value-partition
 Spiciness
 [Mild Medium Hot]
 )

;; so this is nice, but then what about documentation for mild medium and hot?
;; could place all of this onto the main, I suppose. 
(value-partition
 "Part of a value partition describing heat levels in food. 

This value partition consists of three levels

Mild -- less than 1000 on the Scoville Scale
Medium -- between 1000 and 30,000 on the Scoville Scale
Hot -- Above -- 30,000 on the Scoville Scale
"
 Spiciness
 [Mild Medium Hot]
 )

;; this would be the other option. Slightly more verbose. Basically,
;; it would do the disjoints and covering axioms for me. 
(value-partition
 (defclass Spiciness)
 (defoproperty hasSpiciness)
 [declare-classes Mild Medium Hot]
)


(o/save-ontology "pattern.xml" (org.semanticweb.owlapi.io.OWLXMLOntologyFormat.))


;; documentation? Should all share the documentation I think. 
(defclass Spiciness
  
  )

(defoproperty hasSpiciness
  :characteristics functional
  :range Spiciness)

(as-disjoint-subclasses
 Spiciness

 ;; can we formally order these
 ;; do we need the same documentation for all of them, or what?
 (defclass Mild)
 (defclass Medium)
 (defclass Hot)

 )


