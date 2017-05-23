;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, 2017, Newcastle University

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


(ns tawny.display
  (:require [tawny.owl :as o]))

(defn pp-entity [class]
  (str class "\n"))

(defn pp-class [class]
  (str class "\n"))

(defn- pp-subclasses-1 [classlist prepend]
  ;; if there are no subclasses return empty list
  (when-not (zero? (count classlist))
    (str prepend (first classlist) "\n"
         ;; can't use recur, not in tail position
         (pp-subclasses-1 (rest classlist) prepend)
         (pp-subclasses-1 (o/subclasses (first classlist))
                          (str "  " prepend)))))

(defn pp-subclasses [class]
  (str (pp-class class)
       (pp-subclasses-1 (o/subclasses class) "  ")))
