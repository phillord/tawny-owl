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
advantage. 

## Building

Clojure is highly-dynamic, so "build" may be the wrong term. There is not compile-test
