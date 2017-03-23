(ns tawny.pattern-test
  (:use [clojure.test])
  (:require
   [tawny.owl :as o]
   [tawny.debug]
   [tawny.pattern :as p]))


(def to nil)

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/" :prefix "iri" :noname true))))

(defn createandsavefixture[test]
  (let [exp
        #(throw (Exception. "default ontology used"))
        trace
        #(tawny.debug/tracing-println "default ontology used")
        ]
    (when true
      (tawny.util/add-hook
       o/default-ontology-hook exp
       ))
    (when false
      (tawny.util/add-hook
       o/default-ontology-hook trace))
    (createtestontology)
    (test)
    (tawny.util/remove-hook o/default-ontology-hook exp)
    (tawny.util/remove-hook o/default-ontology-hook trace)))

(use-fixtures :each createandsavefixture)


(deftest nil-strip
  (is
   (=
    ;; order is not defined!
    (#'tawny.pattern/nil-strip-hashify
     [:a 1 :b 2 :c nil :d 3 4])
    [:a 1 :b 2 :d 3 4]))
  (is
   (=
    (#'tawny.pattern/nil-strip-hashify
     [:a 1 :b 2 :c 3 nil :d 4 5])
    [:a 1 :b 2 :c 3 :d 4 5])))

(deftest nil-strip-hashify-op
  (is
   (=
    ;; order is not defined!
    (#'tawny.pattern/nil-strip-hashify-op
     ;; have to use real keywords here because op checks for handlers.
     [:subproperty 1 :characteristic :functional :inverse nil :equivalent 3 4])
    [:subproperty 1 :characteristic :functional :equivalent 3 4])))

(defn variadic-identity [& args]
  args)

(deftest p
  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name")
    (p/map->Named {:name "name" :entity ["o" "name"]})

    ))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1)
    (p/map->Named
     {:name "name" :entity ["o" "name" :a 1]})))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1 :b nil :c 2)
    (p/map->Named
     {:name "name" :entity ["o" "name" :a 1 :c 2]})))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1 :b nil :c 2 nil)
    (p/map->Named
     {:name "name"
      :entity ["o" "name" :a 1 :c 2]}))))


(deftest extract-ontology-arg
  (is
   (=
    (#'tawny.pattern/extract-ontology-arg
     [:a 1 :b 2 :ontology "o" :c 3])
    {:ontology "o"
     :args [:a 1 :b 2 :c 3]})))

(deftest value-partition
  (is
   (p/value-partition to
    (o/owl-class to "A")
    [(o/owl-class to "B")
     (o/owl-class to "C")
     (o/owl-class to  "D")])))


(deftest value-partition-strings
  (is
   (p/value-partition to "A" ["B" "C" "D"])))

(deftest partition-values
  (is
   (let [[p _ & v] (p/value-partition to "A" ["B" "C" "D"])]
     (= (sort (p/partition-values to (:entity p)))
        (sort (map :entity v))))))

(deftest as-facet
  (is
   (let [o (o/object-property to "o")
         c (o/owl-class to "c")]
     (p/as-facet to o c)
     (=
      (list (o/owl-some o c))
      (p/facet to c)))))

(deftest pattern-annotator
  (is
   (=
    (p/pattern-annotator to (list (o/owl-class to "c")))
    (list (o/owl-class to "c")))))

(deftest pattern-funcs
  (is
   (seq
    (p/which-pattern
     to
     (first
      (p/pattern-annotator
       to
       (list (o/owl-class to "c")))))))
  (is
   (seq
    (apply
     (partial p/shared-pattern to)
     (p/pattern-annotator
      to (list (o/owl-class to "c")
               (o/owl-class to "d"))))))
  (is
   (let [c1 (o/owl-class to "c1")
         c2 (o/owl-class to "c2")
         c3 (o/owl-class to "c3")]
     (= (set
         (p/pattern-annotator
          to
          (list c1 c2 c3)))
        (set (p/pattern-entities
              to
              (first (p/which-pattern to c1))))))))
