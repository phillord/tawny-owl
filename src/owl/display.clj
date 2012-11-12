(ns owl.display
  (:require [owl.owl :as o]))

(defn pp-entity [entity]
  (str class "\n"))

(defn pp-class [class]
  (str class "\n"))

(defn- pp-subclasses-1 [classlist prepend]
  ;; if there are no subclasses return empty list
  (when-not (= 0 (count classlist))
    (str prepend (first classlist) "\n"
         ;; can't use recur, not in tail position
         (pp-subclasses-1 (rest classlist) prepend)
         (pp-subclasses-1 (o/isubclasses (first classlist))
                          (str "  " prepend)))))

(defn pp-subclasses [class]
  (str (pp-class class)
       (pp-subclasses-1 (o/isubclasses class) "  ")))
