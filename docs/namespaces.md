Namespaces
==========

## Introduction

Tawny-OWL consists of a number of different namespaces, some of which are
meant for end-user use. Here we describe these purpose of each of these. 


* `core.clj` -- currently has no function, but will contain provide a command
  line interface at some point
* `display.clj` -- currently minimal functionality for pretty printing
  OWLObjects. 
* `lookup.clj` -- find symbols being used by tawny-owl to store ontology
  terms. This functionality is heavily used by `memorise.clj', `render.clj`
  but is less use for the end user. 
* `memorise.clj` -- record the currently used symbols and the IRIs that they
  map to. This can help to protect against change in ontologies that are
  accessed via an OWL file. 
* `owl.clj` -- enables building entities described in the core OWL
  specification. 
* `pattern.clj` -- common ontology design patterns. 
* `polyglot.clj` -- enables reading ontology labels from a properties file
  which is particularly useful for multilingual ontologies. 
* `read.clj` -- read an ontology from an OWL file into the current namespace. 
* `reasoner.clj` -- access to a reasoning services, including appropriate
  predicates for testing class inference. Currently supports
  either [ELK](http://code.google.com/p/elk-reasoner/) or
  [HermiT](http://hermit-reasoner.com/).
* `render.clj` -- given an OWLObject, render it as a lisp form that would
  define it. This is not the same thing as the source lookup, since the
  OWLObject might be generated in many ways by tawny, or read in from a file. 
* `repl.clj` -- functions for working in the REPL. This also hooks into the
  `clojure.repl` facilities, giving improved documentation lookup. 
* `util.clj` -- embarrassing for an ontologist to admit, but this is the
  miscellaneous category.
  
  
