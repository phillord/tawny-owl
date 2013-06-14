(ns tawny.debug
  (:use [tawny.util]
        [tawny.owl]))

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
                ["leiningen" "clojure"])
         ]
      (printf "\tfn:%s:cl:%s:ln:%s\n"
              (.getFileName l)
              (.getClassName l)
              (.getLineNumber l)))))

(defn hook-default []
  (reset! default-ontology-hook [])
  (add-hook default-ontology-hook
            #(tracing-println "default ontology used")))

