(defproject uk.org.russet/tawny-owl "0.9-SNAPSHOT"
  :description "tawny-owl enables building OWL ontologies in a programmatic environment."
  :url "http://www.russet.org.uk/blog/tawny-owl"
  :repositories [["maven" "http://repo1.maven.org/maven2"]

                 ;; hermit comes from here
                 ["phillord" "http://homepages.cs.ncl.ac.uk/phillip.lord/maven"]
                 ]

  :resource-paths ["src/resources"]
  
  :scm {:url "https://github.com/phillord/tawny-owl.git"
        :name "git"} 
  
  :license {:name "LGPL"
            :url "http://www.gnu.org/licenses/lgpl-3.0.txt"
            :distribution :repo}

  :dependencies [
                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-api "3.4.3"]
                 [net.sourceforge.owlapi/owlapi-apibinding "3.4.3"]
                 [net.sourceforge.owlapi/owlapi-distribution "3.4.3"]
                 
                 ;; clojure
                 [org.clojure/clojure "1.4.0"]
                 
                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.3.1"]
                 [org.semanticweb.hermit/HermiT "1.3.6.1"]
                 
                 ;; need to access log4j to control elk. 
                 [log4j/log4j "1.2.14"]])
