(use 'tawny.owl)
(use 'tawny.repl)

(defn render-sio []
  (tawny.repl/render-ontology
   (tawny.repl/load-ontology
    (iri
     (clojure.java.io/resource "sio.owl")))
   "dev-resources/sio_render_bench.clj"
   {:terminal :iri}))

(comment
  (render-sio))


(defontology o)

(time
 (load-file "dev-resources/sio_render_bench.clj"))
