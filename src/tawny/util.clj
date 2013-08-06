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

(ns tawny.util
  (:import [java.io Writer]))


;;
;; Utillity functions for improving syntax
;;
(defn groupify-by
  "Takes a list by and turns adjacent values for which pred returns false into
a single list, while leaving values for which pred returns true untouched.
Values for which pred returns true must be separated by at least other value."
[pred list]
  (doall
   (map
    (fn [x]
      (if (-> x (first) (pred))
        (if (> (count x) 1)
          (throw (IllegalArgumentException.
                  (str "Sequence not legal in this context:" x)))
          (first x))
        x))
    (partition-by pred list))))


(defn groupify-at
  "Takes a list and returns all adjacent values which are not in keys into a
single list. Keys must be separated by at least one value."
  [keys list]
  (groupify-by (fn [x] (some #(= x %) keys)) list))

(defn hashify-at
  "Takes a list with alternating keyword values and returns a hash"
  [keys list]
  (apply
   hash-map (groupify-at keys list)))

(defn groupify
  "Takes a list with keywords and values and turns all adjacent values into a
  single list"
  [list]
  (groupify-by keyword? list))

(defn hashify
  "Takes a list with alternating keyword values and returns a hash"
  [list]
  (apply
   hash-map (groupify list)))

;; contains really doesn't do what you expect
(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn has-keys
  "Returns true iff hash has only thekeys in hash."
  [hash thekeys]
  (every?
   (fn [x]
     (in? thekeys x))
   (keys hash)))

(defn check-keys
  "Returns the hash or throws IllegalArgumentException if has does not have
  only thekeys in hash"
  [hash thekeys]
  (when-not (has-keys hash thekeys)
    (throw
     (IllegalArgumentException.
      (format "Expected only keys %s; Got %s" thekeys (keys hash)))))
  hash)

(defmacro quote-word [& symbols]
  `(do
     (list
      ~@(map
         (fn [symbol]
           (name symbol))
         symbols))))


;; hook system
(defn make-hook []
  (atom []))

(defn add-hook [hook func]
  (do
    (when-not
        (some #{func} @hook)
      (swap! hook conj func))
    @hook))

(defn remove-hook [hook func]
  (swap! hook
         (partial
          remove #{func})))

(defn clear-hook [hook]
  (reset! hook []))

(defn run-hook
  ([hook]
     (doseq [func @hook] (func)))
  ([hook & rest]
     (doseq [func @hook] (apply func rest))))


;; unlazy map function
(defn domap
  "Unlazy map function, for when the map function has side effects.

Typing (doall (map)) all the time is hard work!"
  [& body]
  (doall (apply map body)))


(defmacro dofor
  "Unlazy dofor, for when the side effects are necessary.
Unlike doseq this holds onto the head."
  [& body]
  `(doall
    (for ~@body)))

(defn vectorize
  "Given (f [x y]), return another function (g [x & rest]), where items in
rest can be any tree structure, then, f with x and all values in rest. "
  [f]
  (fn [x & args]
    (when-not (seq args)
      (throw (clojure.lang.ArityException. 1 "Expects at least 2 args")))
    (doall
     (map
      (partial f x)
      (filter (comp not nil?) (flatten args))))))




;; on
(defn on [val f]
  (when val
    (f val)))
