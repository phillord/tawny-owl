(ns owl.test.util
  [:require [clojure.test :refer :all] [owl.util :as u]])


(deftest groupify
  (is
   (empty? (u/groupify '())))
  (is
   (= (u/groupify '(:a 1))
      '(:a (1))))
  (is
   (thrown? IllegalArgumentException
            (u/groupify '(:a :b)))))


(deftest hashify
  (is
   (= (u/hashify '(:a 1))
      {:a '(1)}))
  (is
   (=
    (u/hashify '(:a 1 :b 2))
    {:a '(1) :b '(2)})))
  

(deftest check-keys []
  (is
   (= nil (u/check-keys nil [:a :b])))
  (is
   (= {} (u/check-keys {} [:a :b])))
  (is
   (= {:a 1} (u/check-keys {:a 1} [:a :b])))
  (is
   (= {:a 1 :b 2} (u/check-keys {:a 1 :b 2} [:a :b])))
  (is
   (= {:b 2 :a 1} (u/check-keys  {:b 2 :a 1} [:a :b])))
  (is
   (thrown? IllegalArgumentException
               (u/check-keys {:a 1 :b 2 :c 3} [:a :b]))))