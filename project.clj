(defproject uk.org.russet/tawny-owl "1.3.1-SNAPSHOT"
  :description "tawny-owl enables building OWL ontologies in a programmatic environment."
  :url "http://www.russet.org.uk/blog/tawny-owl"
  :repositories [["maven" "http://repo1.maven.org/maven2"]]

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

  :dependencies [
                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-distribution "3.5.0"]

                 ;; clojure
                 [org.clojure/clojure "1.6.0"]
                 [org.clojure/core.logic "0.8.8"]

                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.1"]
                 [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.4"]
                 [net.sourceforge.owlapi/jfact "1.2.2"]

                 ;; need to access log4j to control elk.
                 [log4j/log4j "1.2.17"]

                 ;; for oops delpoyment
                 [org.clojure/data.xml "0.0.8"]
                 [clj-http "1.0.0"]

                 ]

  ;; multiple profiles for testing against different OWL versions. The :base
  ;; dependency adds dev-resources to the path which I need for testing.
  :profiles
  {
   :1.7
   [:base
    {:dependencies [[org.clojure/clojure "1.7.0-beta3"]]}]

   :3.5.0
   [:base
    {:dependencies [[net.sourceforge.owlapi/owlapi-distribution "3.5.0"]]}]

   :3.4.10
   [:base
    {:dependencies [[net.sourceforge.owlapi/owlapi-distribution "3.4.10"]]}]

   :3.4.9
   [:base
    {:dependencies [[net.sourceforge.owlapi/owlapi-distribution "3.4.9"]
                    [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.3"]]}]

   :3.4.8
   [:base
    {:dependencies [[net.sourceforge.owlapi/owlapi-distribution "3.4.8"]
                    [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.2"]
                    ]}]

   ;; compatability with 3.4.5 and earlier are broken for datatype properties.
   }
)
