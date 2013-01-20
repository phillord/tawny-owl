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
