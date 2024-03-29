(defproject uk.org.russet/tawny-owl "2.3.4-SNAPSHOT"
  :description "tawny-owl enables building OWL ontologies in a programmatic environment."
  :url "http://www.russet.org.uk/blog/tawny-owl"
  :repositories [["maven" "https://repo1.maven.org/maven2"]]

  :scm {:url "https://github.com/phillord/tawny-owl.git"
        :name "git"}

  :license {:name "LGPL"
            :url "http://www.gnu.org/licenses/lgpl-3.0.txt"
            :distribution :repo}

  :repl-options {
                 ;; This expression will run when first opening a REPL, in the
                 ;; namespace from :init-ns or :main if specified
                 ;;:init (require 'tawny.repl)
                 }

  :test-selectors {:slow :slow
                   :commit (complement :slow)}

  :jar-exclusions [#".*\.org"
                   #".*\.html"
                   #"tawny/\.dir-locals.el"
                   #"tawny/ChangeLog"
                   #"tawny/temp.*\."
                   ]

  :dependencies [
                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-distribution "4.5.26"]

                 ;; clojure
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/core.logic "1.0.1"]

                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.3"]
                 [net.sourceforge.owlapi/org.semanticweb.hermit "1.3.8.413"]
                 [net.sourceforge.owlapi/jfact "4.0.2"
                  :exclusions [net.sourceforge.owlapi/owlapi-apibinding]]

                 ;; I have to explicitly include several dependencies
                 ;; specifically so I can switch the logging off. How does
                 ;; this make sense?

                 ;; Shut up ELK
                 [org.apache.logging.log4j/log4j "2.21.1" :extension "pom"]
                 ;; Shut up OWL API
                 [org.slf4j/slf4j-nop "2.0.9"]

                 ;; identitas
                 [uk.org.russet/identitas-j "0.0.1"]]

  ;; multiple profiles for testing against different OWL versions. The :base
  ;; dependency adds dev-resources to the path which I need for testing.
  :profiles
  {
   :1.10
   [:base
    {:dependencies [[org.clojure/clojure "1.10.0"]]}]
   :1.9
   [:base
    {:dependencies [[org.clojure/clojure "1.9.0"]]}]
   })
