(defproject uk.org.russet/clojure-owl "1.0.0-SNAPSHOT"
  :description "Help to define a project in OWL"
  :url "http://code.google.com/p/clojure-owl/"
  :plugins [[lein-swank "1.4.4"]]
  :repositories [["maven" "http://repo1.maven.org/maven2"]
                 ;; getting hermit from here
                 ["berkley" "http://code.berkeleybop.org/maven/repository/"]
                 ]
  :dependencies [
                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-api "3.4.1"]
                 [net.sourceforge.owlapi/owlapi-apibinding "3.4.1"]
                 [net.sourceforge.owlapi/owlapi-distribution "3.4.1"]

                 ;; clojure
                 [org.clojure/clojure "1.4.0"]
                 
                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.3.1"]
                 ;;[org.semanticweb.hermit/HermiT "1.0-SNAPSHOT"]
                 ;;[org.semanticweb./HermiT "1.3.6-BBOP"]
                 

                 ]





)