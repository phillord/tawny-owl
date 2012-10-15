(ns owl.test.owl
  [:require [owl [owl :as owl]]]
  [:use clojure.test])



(deftest defontologytest
  (is (not (nil?
            (owl/defontology testontology
              :file "test.omn" :iri "http://iri/")))))
