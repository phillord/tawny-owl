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

  :managed-dependencies [[com.google.guava/guava "33.0.0-jre"]
                         [com.google.code.findbugs/jsr305 "3.0.2"]
                         [org.slf4j/slf4j-api "2.0.11"]]

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
                 [net.sourceforge.owlapi/owlapi-distribution "5.5.1"]

                 ;; clojure
                 [org.clojure/clojure "1.12.4"]
                 [org.clojure/core.logic "1.1.1"]

                 ;; reasoners
                 [io.github.liveontologies/elk-owlapi "0.6.0"]
                 [net.sourceforge.owlapi/org.semanticweb.hermit "1.4.5.519"]
                 [net.sourceforge.owlapi/jfact "5.0.3"
                  :exclusions [net.sourceforge.owlapi/owlapi-apibinding]]

                 ;; Shut up OWL API and ELK
                 [org.slf4j/slf4j-nop "2.0.9"]

                 ;; identitas
                 [uk.org.russet/identitas-j "0.0.1"]]

  ;; multiple profiles for testing against different OWL versions. The :base
  ;; dependency adds dev-resources to the path which I need for testing.
  :profiles
  {
   :1.12
   [:base
    {:dependencies [[org.clojure/clojure "1.12.4"]]}]
   :1.10
   [:base
    {:dependencies [[org.clojure/clojure "1.10.0"]]}]
   :1.9
   [:base
    {:dependencies [[org.clojure/clojure "1.9.0"]]}]
   })
