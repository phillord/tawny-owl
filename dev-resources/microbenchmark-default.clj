(defn dull
  ([])
  ([a])
  ([a b])
  ([a b c])
  ([a b c d])
  ([a b c d e])
  ([a b c d e f])
  ([a b c d e f g])
  ([a b c d e f g & r]))

(use 'criterium.core)
(use 'tawny.owl)

(defontology o)

(println "Base line")
(bench (dull 1 2))
(bench (dull 1 2 3 4 5))

(println "get-current-ontology base line for smallest add")
(bench (get-current-ontology))
(bench (get-current-ontology-maybe))

(println "default ontology 2 args, with and without ontology")
(bench (default-ontology dull 1 2))
(bench (default-ontology dull o 1 2))

(println "\ndefault ontology 5 args, with and without ontology")
(bench (default-ontology dull 1 2 3 4 5))
(bench (default-ontology dull o 1 2 3 4 5))

(println "broadcast ontology 2 args, with and without ontology")
(bench (broadcast-ontology dull 1 2))
(bench (broadcast-ontology dull o 1 2))

(println "broadcast ontology 5 args, with and without ontology")
(bench (broadcast-ontology dull 1 2 3 4 5))
(bench (broadcast-ontology dull o 1 2 3 4 5))


(println "Big O performance: default")
(bench (default-ontology dull o 1 2))
(bench (default-ontology dull o 1 2 3))
(bench (default-ontology dull o 1 2 3 4))
(bench (default-ontology dull o 1 2 3 4 5))
(bench (default-ontology dull o 1 2 3 4 5 6))
(bench (default-ontology dull o 1 2 3 4 5 6 7))
(bench (default-ontology dull o 1 2 3 4 5 6 7 8))

(println "Big O performance: broadcast")
(bench (broadcast-ontology dull o 1 2))
(bench (broadcast-ontology dull o 1 2 3))
(bench (broadcast-ontology dull o 1 2 3 4))
(bench (broadcast-ontology dull o 1 2 3 4 5))
(bench (broadcast-ontology dull o 1 2 3 4 5 6))
(bench (broadcast-ontology dull o 1 2 3 4 5 6 7))
(bench (broadcast-ontology dull o 1 2 3 4 5 6 7 8))
