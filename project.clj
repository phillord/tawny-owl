(defproject uk.org.russet/tawny-owl "1.5.0"
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

  :jar-exclusions [#".*\.org"
                   #".*\.html"
                   #"tawny/\.dir-locals.el"
                   #"tawny/ChangeLog"
                   #"tawny/temp.*\."
                   ]

  :dependencies [

                 ;; http://search.maven.org/#artifactdetails%7Cnet.sourceforge.owlapi%7Corg.semanticweb.hermit%7C1.3.8.413%7Cbundle
                 ;; http://search.maven.org/#artifactdetails%7Cnet.sourceforge.owlapi%7Cpellet-parent-ignazio1977%7C2.4.0-ignazio1977%7Cpom

                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-distribution "4.1.4"]

                 ;; clojure
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/core.logic "0.8.10"]

                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.3"]
                 [net.sourceforge.owlapi/org.semanticweb.hermit "1.3.8.413"]
                 [net.sourceforge.owlapi/jfact "4.0.2"
                  :exclusions [net.sourceforge.owlapi/owlapi-apibinding]]

                 ;; need to access log4j to control elk.
                 [log4j/log4j "1.2.17"]]

  ;; multiple profiles for testing against different OWL versions. The :base
  ;; dependency adds dev-resources to the path which I need for testing.
  :profiles
  {
   :1.8-direct
   [:base
    {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]
     :dependencies [[org.clojure/clojure "1.8.0"]]}]

   :1.8
   [:base
    {:dependencies [[org.clojure/clojure "1.8.0"]]}]

   :1.7
   [:base
    {:dependencies [[org.clojure/clojure "1.7.0"]]}]

   }
)
