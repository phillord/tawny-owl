(defproject uk.org.russet/tawny-owl "1.0-rc1"
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

  :dependencies [
                 ;; owl API
                 [net.sourceforge.owlapi/owlapi-api "3.4.8"]
                 [net.sourceforge.owlapi/owlapi-apibinding "3.4.8"]
                 [net.sourceforge.owlapi/owlapi-distribution "3.4.8"]

                 ;; clojure
                 [org.clojure/clojure "1.5.1"]

                 ;; reasoners
                 [org.semanticweb.elk/elk-owlapi "0.4.1"]
                 [com.hermit-reasoner/org.semanticweb.hermit "1.3.8.2"]

                 ;; need to access log4j to control elk.
                 [log4j/log4j "1.2.17"]])
