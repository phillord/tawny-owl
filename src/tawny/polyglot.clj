;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2013, Phillip Lord, Newcastle University

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


(ns tawny.polyglot
  (:require [tawny.lookup] [tawny.owl]))


;; oh dear, lifted from memorise -- move to lookup I think
(defn- var-str 
  "Given a var, return a string representation of its name"
  [var]
  (str (:name (meta var))))


;; function to create empty properties file
(defn polyglot-create-resource [filename] 
  (with-open [w (clojure.java.io/writer filename)]
    (doseq [name 
            (into (sorted-set)
                  (for [[k v] (tawny.lookup/iri-to-var *ns*)]
                    (var-str v)))]
      (.write w (format "%s=\n" name)))))

;; main entry function to gather add labels to all classes, given a language,
;; warning of missing translations.

(defn polyglot-load-label [filename locale]
  (if-let [resource
           (clojure.java.io/resource filename)]
    (let [props 
          (with-open 
              [r (clojure.java.io/reader 
                  resource)]
            (let [props (java.util.Properties.)]
              (.load props r)
              props)
            )
          ]
      (doseq [[k v] (tawny.lookup/name-to-var *ns*)]
        ;; when there is a label
        (let [label (.getProperty props k)]
          (if (seq label)
            (tawny.owl/add-annotation
             (var-get v)
             (list (tawny.owl/label label locale)))
            (println
             (format "Missing Label (%s:%s)"
                     k locale))))))
    (throw (IllegalStateException. 
            (format "Cannot find properties file: %s" filename)))))


