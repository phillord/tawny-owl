(ns tawny.debug)

(defn tracing-println [& args]
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
