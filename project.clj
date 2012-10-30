(defproject owl "1.0.0-SNAPSHOT"
  :description "Help to define a project in OWL"
  :plugins [[lein-swank "1.4.4"]]
  :repositories [["maven" "http://repo1.maven.org/maven2"]]
  :dependencies [[net.sourceforge.owlapi/owlapi-api "3.4.1"]
                 [net.sourceforge.owlapi/owlapi-apibinding "3.4.1"]
                 [net.sourceforge.owlapi/owlapi-distribution "3.4.1"]

                 [org.clojure/clojure "1.4.0"]
                 
                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.3.1"]])





