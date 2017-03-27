(ns tawny.test-util
  (:require [tawny.util :as sut]
            [tawny.query :as q]
            [clojure.test :as t]))

(defmacro is-set= [constructor query]
  `(let [a# ~constructor]
     (t/is
      (=
       ~query
       #{a#}))))

(defmacro is-set-not= [constructor query]
  `(let [a# ~constructor]
     (t/is
      (not
       (=
        ~query
        #{a#})))))

(defn as-name-set [entities]
  (sut/set-map q/tawny-name entities))
