;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.

(ns tawny.debug)

(defn tracing-println [& _]
  (let [e (Exception.)
        st (.getStackTrace e)
        ]
    (println "Default ontology used")
    (doseq
        [l (take 30 st)
         :when (every?
                #(not
                  (.contains
                   (.getClassName l) %))
                [
                 "tawny.owl$default_ontology"
                 "tawny.util$run_hook" "tawny.owl_test$createandsavefixture$"
                 "tawny.debug" "leiningen" "clojure"])
         ]
      (printf "\tfn:%s:cl:%s:ln:%s\n"
              (.getFileName l)
              (.getClassName l)
              (.getLineNumber l)))))
