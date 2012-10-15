(ns owl.test.util
  [:require [owl [util :as util]]]
  [:use clojure.test])


(deftest groupify
  (is
   (= (util/groupify '(:a 1))
      '(:a (1))))

  (is
   (thrown? IllegalArgumentException
            (util/groupify '(:a :b))))
  )

(deftest hashify
  (is
   (= (util/hashify '(:a 1))
      {:a '(1)}))
   
  (is
   (=
    (util/hashify '(:a 1 :b 2))
    {:a '(1) :b '(2)}))
  )
  
