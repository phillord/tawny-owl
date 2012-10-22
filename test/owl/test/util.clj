(ns owl.test.util
  [:require [clojure.test :refer :all] [owl.util :as u]])




(deftest groupify
  (is
   (= (u/groupify '(:a 1))
      '(:a (1))))

  (is
   (thrown? IllegalArgumentException
            (u/groupify '(:a :b))))
  )

(deftest hashify
  (is
   (= (u/hashify '(:a 1))
      {:a '(1)}))
   
  (is
   (=
    (u/hashify '(:a 1 :b 2))
    {:a '(1) :b '(2)}))
  )
  
