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

(defn- merge-hash-map
  "Give a list turn into a map like apply hash-map, but assumes that the
values are lists and concats duplicates."
  [& list]
  (reduce
   (partial merge-with concat)
   (map
    (fn [n]
      (apply hash-map n))
    (partition 2 list))))

(defn hashify-at
  "Takes a list with alternating keyword values and returns a hash"
  [keys list]
  (apply
   merge-hash-map (groupify-at keys list)))

(defn groupify
  "Takes a list with keywords and values and turns all adjacent values into a
  single list"
  [list]
  (groupify-by keyword? list))

(defn hashify
  "Takes a list with alternating keyword values and returns a hash"
  [list]
  (apply
   merge-hash-map (groupify list)))

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

(defmacro quote-word
  "Given a list of symbols, return a list of names."
  [& symbols]
  `(do
     (list
      ~@(map
         (fn [symbol]
           (name symbol))
         symbols))))

;; hook system
(defn make-hook
  "Make a hook."
  []
  (atom []))

(defn add-hook
  "Add func to hook."
  [hook func]
  (do
    (when-not
        (some #{func} @hook)
      (swap! hook conj func))
    @hook))

(defn remove-hook
  "Remove func from hook."
  [hook func]
  (swap! hook
         (partial
          remove #{func})))

(defn clear-hook
  "Empty the hook."
  [hook]
  (reset! hook []))

(defn run-hook
  "Run the hook with optional arguments. Hook functions are run in the order
that they were added."
  ([hook]
     (doseq [func @hook] (func)))
  ([hook & rest]
     (doseq [func @hook] (apply func rest))))


;; unlazy map function
(defn ^java.util.Collection domap
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
(defn on
  "Call f on val if val is not null."
  [val f] 
 (when val
    (f val)))

;; prepare for some strange macros

;; Taken from https://github.com/mikera/vectorz-clj
;; How does it make sense to have a macro which expands to something like
;; (let [x y] x)
;; which surely should be a non-op?
(defmacro tag-symbol [tag form]
  "Given form, return the same form but with attached :tag metadata
defining the return type of that form."
  ;; create a local tagged-sym which is a gensym but with metadata attached.
  (let [tagged-sym (vary-meta (gensym) assoc :tag tag)]
    `(let [~tagged-sym ~form] ~tagged-sym)))

(defmacro with-types
  [spec & body]
  "Given a spec of the form [symbol [types]] evaluate body if
and only the value of symbol is an instance? of one of types. The symbol is
type-hinted to the type of the first of types to match.

The main aim for this is to avoid reflective calls to overloaded methods in
Java. So if we have an overloaded static method call:

C.met(String)
C.met(Double)
C.met(Boolean)

  (with-types [val [String Double Boolean]]
    (C/met val)

will call met without reflection, so long as val is one of String, Double or
Boolean. If none of the types are matched, an IllegalArgumentException will be
thrown."
  (let [hint-var# (first spec)
        types# (second spec)]
    (if (seq types#)
      (let [type# (first types#)]
        `(let [~hint-var#
               (tawny.util/tag-symbol ~type# ~hint-var#)]
           (if (instance? ~type# ~hint-var#)
             ~@body
             (with-types
               [~hint-var# ~(vec (rest types#))]
               ~@body))))
      `(throw (IllegalArgumentException. "No types have been matched")))))
