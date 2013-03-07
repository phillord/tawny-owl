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

    (defproject pizza "0.1-SNAPSHOT"
      :description "The Pizza Ontology in tawny-owl"
      :dependencies [[org.clojure/clojure "1.4.0"]
                     [uk.org.russet/tawny-owl "0.9"]
                     ]
    
      :main pizza.core
    )
    
The project definition is itself written in Clojure; in most cases Tawny
projects will require just Clojure and Tawny as dependencies (the version
numbers of these will change!). The description, and project name will, of
course be unique to your project.

## File Locations

Clojure uses namespaces to avoid name clashes. Although less rigid than Java,
most often a clojure namespace maps to a single file. Leiningen expects files
at a specific location. All sources files are held in, unsurprisingly, the
`src` directory. Directories below this create a namespace hierarchy. So, the
main source for the pizza ontology is found at `src/pizza`. 

## Evaluating a file

The first thing that you need to do with a Tawny ontology is evaluate a file;
that is, have Clojure interpret all the statements, and turn them into an
something that you can interact with. Unfortunately, the different
environments that you might choose do this differently. However, the are
plenty of tutorials; for instance I use 
[emacs](http://clojure-doc.org/articles/tutorials/emacs.html). 

## Building an ontology

This is the main thing that you may want to do with Tawny, and it is covered
in its own [documentation](getting-start.md). 

## A main method

You don't need a main function for an ontology, but it can be useful. For
instance, tawny-pizza includes a simple main, that writes the ontology in a
couple of different formats. The practical upshot of this is that if you
generate an single jar file for a project, running this file by, for instance,
clicking on it will generate OWL files. 

Previously, we saw a project definition including the statement `:main
pizza.core`. This describes the namespace that the main method is expected to
appear in. In this file (`src/pizza/core.clj`), we find a namespace
declaration. 

     (ns pizza.core
       (:use [tawny.owl] 
             [pizza.pizza])
       (:gen-class)
       )

and a simple main method. In practice, a main method isn't that useful, but it
will be run whenever the project jar is clicked on. In this case, the method
simply saves the ontology out in a couple of formats.

     (defn -main [& args]
       (with-ontology pizzaontology
         (save-ontology "pizza.rdf" :rdf)
         (save-ontology "pizza.owl" :owl)))
     
     
## Publishing an Ontology 

With Tawny, there are three ways to publish an ontology. First, an ontology
developed in Tawny can be written out as OWL, in any of many formats, using
the `save-ontology` function, which is covered in more detail in the
[getting-started](getting-started.md) documentation. Anyone using an OWL
ontology will be able to consume this form, including another Tawny user.
Secondly, it is possible to release the Tawny-OWL files themselves as source.
This will be useful to other Tawny-OWL users, but is only really necessary if
they wish to modify the ontology; it is best accomplished through a version
control system. Finally, the Tawny-OWL files can be packaged as a jar file and
published to a maven repository. This is the best way to share an ontology
with other Tawny-OWL users who wish to use, but not change your ontology. The
most straight-forward to do this is through the
[Clojars](http://www.clojars.org) repository; there is a good
[tutorial](https://github.com/ato/clojars-web/wiki/tutorial) available for
this. 
