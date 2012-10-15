(ns owl.test.owl-test
  [:require [owl [owl :as owl]]]
  [:use clojure.test])


(deftest groupify
  (is
   (= (owl/groupify '(:a 1))
      '(:a (1))))

  (is
   (thrown? IllegalArgumentException
            (owl/groupify '(:a :b))))
  )

(deftest hashify
  (is
   (= (owl/hashify '(:a 1))
      {:a '(1)}))
   
  (is
   (=
    (owl/hashify '(:a 1 :b 2))
    {:a '(1) :b '(2)}))
  )
  

(deftest create-ontology
  
  

  )


  