(defproject uk.org.russet/tawny-owl "2.0.1-SNAPSHOT"
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
                 [net.sourceforge.owlapi/owlapi-distribution "4.5.8"]

                 ;; clojure
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.logic "0.8.11"]

                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.3"]
                 [net.sourceforge.owlapi/org.semanticweb.hermit "1.3.8.413"]
                 [net.sourceforge.owlapi/jfact "4.0.2"
                  :exclusions [net.sourceforge.owlapi/owlapi-apibinding]]

                 ;; I have to explicitly include several dependencies
                 ;; specifically so I can switch the logging off. How does
                 ;; this make sense?

                 ;; Shut up ELK
                 [log4j/log4j "1.2.17"]
                 ;; Shut up OWL API
                 [org.slf4j/slf4j-nop "1.7.10"]

                 ;; identitas
                 [uk.org.russet/identitas-j "0.0.1"]]

  ;; multiple profiles for testing against different OWL versions. The :base
  ;; dependency adds dev-resources to the path which I need for testing.
  :profiles
  {
    :1.9
   [:base
    {:dependencies [[org.clojure/clojure "1.9.0"]]}]
   })
