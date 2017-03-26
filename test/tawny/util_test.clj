;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

(ns tawny.util-test
  [:require [clojure.test :refer :all] [tawny.util :as u]])


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

(deftest except-at
  (is
   (=
    (u/groupify-except-at #{} '(:a 1 :b 2 3 :c 4 5 6))
    '(:a (1) :b (2 3) :c (4 5 6))))
  (is
   (thrown?
    IllegalArgumentException
    (u/groupify-except-at #{} '(:a 1 :b 2 3 :d :c 4 5 6))))
  (is
   (=
    (u/groupify-except-at #{:d} '(:a 1 :b 2 3 :d :c 4 5 6))
    '(:a (1) :b (2 3 :d) :c (4 5 6))))
  (is
   (thrown?
    IllegalArgumentException
    (u/hashify-except-at #{} '(:a 1 :b 2 3 :d :c 4 5 6))))
  (is
   (=
    (u/hashify-except-at #{:d} '(:a 1 :b 2 3 :d :c 4 5 6))
    '{:c (4 5 6), :b (2 3 :d), :a (1)}))
  (is
   (=
    (u/hashify-except-at #{} '(1 2 :a 1 :b 2 3))
    '{(1 2) :a, (1) :b})))


(u/groupify-except-at #{:x} '(:a 1))
;; => (:a (1))

(u/groupify-except-at #{:x} '(:a :x))
;; => (:a (:x))

(u/hashify-except-at #{:x} '(:a 1))
;; => {:a (1)}

(u/hashify-except-at #{:x} '(:a :x))
;; => {:a (:x)}

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

(deftest on
  (is
   (nil?
    (u/on nil
        (constantly true))))

  (is
   (true?
    (u/on true
          (constantly true))))
  (is
   (= 10
      (u/on 10
            identity)))
  )


(deftest with-types
  ;; the actual, "don't reflect" bit of this macro is hard to check!
  (is (thrown? IllegalArgumentException
               (let [name "string"]
                 (u/with-types [name []] name))))
  (is (thrown? IllegalArgumentException
               (let [name (Object.)]
                 (u/with-types [name [String]] name))))

  (is
   (let [name "Hello"]
     (u/with-types [name [String]] name))))
