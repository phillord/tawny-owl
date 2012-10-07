
(require 'owl)

(defmacro sav [body]
  `(do
     ~body
     (owl/save-ontology)))

(sav (owl/objectproperty "hasPart"))

(sav (owl/owlclass "test1"
                {:subclass ["test2" "test3"]
                 :equivalent "test6"}))

(sav (owl/owlclass "test10"
                   {:subclass ["hasPart" "test15"]}))

(sav (owl/owlclass "test8"
                   {:subclass ["test9" "test10"]}))

(sav (owl/owlclass "test4"
                   {:subclass "test3"}))

(sav (owl/owlclass "test6"))

(sav (owl/owlclass "test5" {}))

;; hmmm
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass x {:subclass "test3"}))
   '("a" "b" "c" "d"))))
 
(sav
 (dorun
  (map
   (fn [x]
     (owl/owlclass
      (str "HumanChromosome" x)
      {:subclass "HumanChromosome"}))
   (range 1 23))))


(sav (owl/owlclass "test"
                   {:subclass [(owl/some "hasPart" "test2")
                               (owl/some "hasPart" "test3")]}))

(sav
 (owl/owlclass "test3"
               {:subclass (owl/only "hasPart" "test2")}))


(owlclass-without-curlies
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


(defn owl-hashify-frame
  [frame]
  (apply
   hash-map (owl-reduce-frame-1 () frame nil nil)))


(apply hash-map '(:a 1 :b 2 :c 3))
;; returns (:a (1 2 3) :b (1) :c (4 5 6))


(owl-hashify-frame '(:a 1 2 3 :b 1 :c 4 5 6))


(defn owl-reduce-frame-1
  [reduced-frame long-frame-left current-keyword current-val-list]
  ;;(println "Call:" )
  ;;(println "\treduced-frame" reduced-frame)
  ;;(println "\tlong-frame-left" long-frame-left)
  ;;(println "\tcurrent-keyword" current-keyword)
  ;;(println "\tcurrent-val-list" current-val-list)
  (if-let [first-val (first long-frame-left)]
    (do
      ;;(println "first-val:" first-val)
      (if (keyword? first-val)
        (do 
        ;;  (println "is a keyword")
          (if (and current-keyword current-val-list)
            (do
              ;;(println "compressing vals to list")
              (owl-reduce-frame-1
               (cons current-keyword
                     (cons current-val-list
                           reduced-frame))
               (rest long-frame-left) first-val nil))
            (do
              ;;(println "starting new list")
              (owl-reduce-frame-1
               reduced-frame (rest long-frame-left)
               first-val nil))))
        (do
          ;;(println "not a keyword:" current-val-list)
          (if current-val-list
            (owl-reduce-frame-1 reduced-frame (rest long-frame-left)
                              current-keyword (cons first-val current-val-list))
            (owl-reduce-frame-1 reduced-frame (rest long-frame-left)
                              current-keyword (list first-val))))))

    ;; else is nothing remaining in long-frame-left
    (do ;;(println "nothing remaining, stop recursion")
        (cons current-keyword
              (cons current-val-list
                    reduced-frame)))))
        


;; FAILING two keywords
(println "Final:" (owl-reduce-frame-1 () '(:a 1 2 3 :b 4 5) nil nil))

;; one keyword call
(println "Final: " (owl-reduce-frame-1 () '(:a 1 2) nil nil))


;; simplest from expected-entry
(println "Final: " (owl-reduce-frame-1 () '(:a 1 ) nil nil))


;; single recursive call
(println "Final:" (owl-reduce-frame-1 () '(1) :a nil))

;; simplest case with non nil retur val -- recursion terminate cond
(println
 "Final:" (owl-reduce-frame-1 () () :a '(1)))
        
    
    
;; func has parameters
;; frame so far (list)
;; frame-left
;; current-keyword
;; current-val-list

;; if frame-left empty return reduced-frame
;;
;; if first frame-left equals :keyword
;;    if current-keyword and current-val-list
;;        recurse frame-so-far = cons current-keyword cons current-val-list frame-so-far
;;                frame-left = rest frame-left
;;                current-key = first
;;                current-val-list = nil
;; else
;;        recurse frame-so-far = frame-so-far
;;                frame-left = rest frame-left
;;                current-key = current-key
;;                current-val-list = cons first current-val-list

   

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