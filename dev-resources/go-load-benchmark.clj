;; This depends on go_rendered.clj which can be produced from the tawny-go project.






;;comment
(use 'tawny.repl)
(use 'tawny.owl)

(time
 (load-ontology
  (iri (clojure.java.io/resource "go.owl"))))


(defontology ont)

(time
 (load-file "dev-resources/go_rendered.clj"))
