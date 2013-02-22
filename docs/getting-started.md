Getting Started
===============

## Introduction 


This is an introduction to the Tawny-OWL library. It assumes that you have a
working Clojure environment; if not please see a
[getting started with clojure](getting-started-with-clojure.md) documentation.

Tawny-OWL is a library which supports the production of ontologies using the
[Ontology Web Language](http://www.w3.org/TR/owl2-overview/). It is designed
to have a simple syntax, modelled after the
[OWL Manchester Syntax](http://www.w3.org/TR/owl2-manchester-syntax/). At it's
simplest, it provides the ability to write ontologies with a clearly defined
syntax. Individual classes or properties can be added and removed at will, or
changed straight-forwardly, as with a GUI framework. However, as it provides
a readable text syntax, it is more convienient to integrate with, for example,
version control systems; ontologies can be searched, and modified with a
simple search and replace, or more complex regexps and so on. 

However, this does not uncover the main power of Tawny; although it looks like
Manchester syntax, underneath there is a full programming language. It is
possible to extend and build on the core library in arbitrary ways. Many
classes can be created according to a pattern in a single statement; it is
possible to add specialised support for your given domain, provide new syntax
for your purposes. 

In this document, we will explore the basic usage of the library, using
examples from the tawny-pizza -- the traditional
[Pizza Ontology](http://robertdavidstevens.wordpress.com/2010/01/22/why-the-pizza-ontology-tutorial/). 
A working version of this ontology ported to Tawny is
[available](https://github.com/phillord/tawny-pizza). 


## Creating a new ontology

Clojure has a namespacing mechanism which is used to prevent name clashes. A
single declaration 




