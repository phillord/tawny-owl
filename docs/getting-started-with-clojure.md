Getting Started with Clojure
============================

## Introduction 

There are many good introductions to Clojure; for instance see
[Clojure Doc](http://clojure-doc.org/) or 
[Learn Clojure](http://learn-clojure.com). However, most of these are aimed at
learning Clojure as a programming language; at its simplest, Tawny-OWL can be
used as a textual method for writing OWL based ontologies, and understanding
how Clojure works as a programming language is not necessary. This, therefore,
is a high-level overview describing the basic tools that are necessary for
Tawny-OWL use. 

## Reading Tawny-OWL files. 

Clojure is a modern JVM-based Lisp implementation. Clojure, and therefore,
Tawny-OWL files are just text and can, therefore, be read with any
text-editor; although using one which can cope with alternative line-endings
(i.e. not notepad!) will make life easier. 

Building an ontology is, in general, a complex business; in general, it works
better with a good editor or IDE. There are many of these that you can use; I
am a die-hard Emacs fan, but Eclipse, IntelliJ and Netbeans are all good
alternatives.
[Clojure Doc](http://clojure-doc.org/articles/ecosystem/development_tools.html)
has reasonable coverage. 

For those using Emacs, if you are not familiar with lisp I would advice
ignoring the advice of many tutorials to install paredit. It really is good
for Lisp, but Tawny-OWL doesn't really nest very much, so it's not that big an
advantage, and can be disconcerting in first use. 

## Building

Clojure is highly-dynamic, so "build" may be the wrong term. There is no
compile-test cycle with Clojure. However, Tawny-OWL uses "Leiningen" to
perform two tasks. First, it resolves dependencies; any ontology built with
Tawny-OWL will have a large number of these, and they can change over time, so
not maintaining this by hand is a boon. Secondly, Lein provides an easy way to
launch the REPL -- read-eval-print-loop. 

Leiningen can be installed from a package manager, although the most release
(Lein 2) is quite recent and many repositories have the older Lein 1. In this
case, you need to install from the main
[lein website](https://github.com/technomancy/leiningen). Installation is
relatively straight-forward on both Windows and Linux. 

## A project file

Leiningen uses a project file to define the build. The file is called
`project.clj` and it, itself, written in Clojure. A reasonable example for
Tawny-OWL can be see in the
[Tawny Pizza](https://github.com/phillord/tawny-pizza) ontology. 



