
(require 'owl.owl)
(use '[owl.owl :only (defontology)])


;; this bit don't work yet, but I want it to!
(defontology test-ontology
   :file "temp.omn"
   :iri "http://www.semanticweb.org/ontologies/ont.owl"
   :prefix "ont:"
   )


(defmacro sav [body]
  `(do
     ~body
     (owl.owl/save-ontology)))


(sav (owl.owl/owlclass "test1"))

;; this isn't working
(sav (owl.owl/add-class "test1"))
(sav (owl.owl/add-annotation "test1" (owl.owl/label "hello" "en")))

(sav (owl.owl/owlclass "test1"
                   :annotation
                   (owl.owl/comment "comment")
                   (owl.owl/label "label")))

(sav (owl.owl/objectproperty "hasPart"))

(sav (owl.owl/owlclass "test1"
                :subclass "test2" "test3"
                :equivalent "test6"))

(sav (owl.owl/owlclass "test10"
                   :subclass (owl.owl/some "hasPart" "test15")))

(sav (owl.owl/owlclass "test8"
                   :subclass "test9" "test10"))

(sav (owl.owl/owlclass "test4"
                   :subclass "test3"))

(sav (owl.owl/owlclass "test6"))

(sav (owl.owl/owlclass "test5" {}))

;; hmmm
(sav
 (dorun
  (map
   (fn [x]
     (owl.owl/owlclass x :subclass "test3"))
   '("a" "b" "c" "d"))))
 
(sav
 (dorun
  (map
   (fn [x]
     (owl.owl/owlclass
      (str "HumanChromosome" x)
      :subclass "HumanChromosome"))
   (concat '("X" "Y") (range 1 23)))))


(sav (owl.owl/owlclass-explicit "test"
                   {:subclass [(owl.owl/some "hasPart" "test2")
                               (owl.owl/some "hasPart" "test3")]}))

(sav
 (owl.owl/owlclass-explicit "test3"
               {:subclass (owl.owl/only "hasPart" "test2")}))


(owl.owl/owlclass
 "name"
 :subclass "sub1"
 :equivalent "eq1"
 )



(defn owlclass-without-curlies
  [name & frames]
  (owl/owlclass name
                (owl-hashify-frame frames)))



(owl/save-ontology)
(sav (owl.owl/reset-ontology))


;; (owl/owlclass
;;  "test2"
;;  :subclass (owl/only "hasPart" "test2")
;;  )


;;(temp "name"
;;      :subclass "sub1" (owl/only "hasPart" "2")
;;      :equivalent "eq1"
;;      )




           

