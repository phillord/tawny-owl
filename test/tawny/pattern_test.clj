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
    ["name" ["o" "name"]]))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1)
    ["name" ["o" "name" :a 1]]))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1 :b nil :c 2)
    ["name" ["o" "name" :a 1 :c 2]]
    ))

  (is
   (=
    (#'tawny.pattern/p variadic-identity "o" "name" :a 1 :b nil :c 2 nil)
    ["name" ["o" "name" :a 1 :c 2]])))


(deftest extract-ontology-arg
  (=
   (#'tawny.pattern/extract-ontology-arg
    [:a 1 :b 2 :ontology "o" :c 3])
   {:ontology "o"
    :args [:a 1 :b 2 :c 3]}))
