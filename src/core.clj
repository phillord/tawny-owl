
(require 'owl)

(defmacro sav [body]
  `(do
     ~body
     (owl/save-ontology)))

(sav (owl/objectproperty "hasPart"))

(sav (owl/owlclass "test1"
                :subclass "test2" "test3"
                :equivalent "test6"))

(sav (owl/owlclass "test10"
                   :subclass (owl/some "hasPart" "test15")))

(sav (owl/owlclass "test8"
                   :subclass "test9" "test10"))

(sav (owl/owlclass "test4"
                   :subclass "test3"))

(sav (owl/owlclass "test6"))

(sav (owl/owlclass "test5" {}))

;; hmmm
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass x :subclass "test3"))
   '("a" "b" "c" "d"))))
 
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass
      (str "HumanChromosome" x)
      :subclass "HumanChromosome"))
   (concat '("X" "Y") (range 1 23)))))


(sav (owl/owlclass "test"
                   {:subclass [(owl/some "hasPart" "test2")
                               (owl/some "hasPart" "test3")]}))

(sav
 (owl/owlclass "test3"
               {:subclass (owl/only "hasPart" "test2")}))


(owl/owlclass
 "name"
 :subclass "sub1"
 :equivalent "eq1"
 )



(defn owlclass-without-curlies
  [name & frames]
  (println "Name" name )
  (println "Frame" (owl-hashify-frame frames))
  (owl/owlclass name
                (owl-hashify-frame frames)))



(owl/save-ontology)
(sav (owl/reset-ontology))


;; (owl/owlclass
;;  "test2"
;;  :subclass (owl/only "hasPart" "test2")
;;  )


;;(temp "name"
;;      :subclass "sub1" (owl/only "hasPart" "2")
;;      :equivalent "eq1"
;;      )




           

;; (defn temp
;;   [name & frames]
;;   (println name)
;;   (binding [framemap {}
;;             key nil
;;             val nil]
;;     (dorun
;;      (map
;;       (fn [arg]
;;         (if (keyword? arg)
;;           (do (set! key arg))
;;           (do (set! val (cons arg val)))
;;       frames))))))q



;;           (cons 1 '(2))


