Getting Started
===============

## Introduction


This is an introduction to the Tawny-OWL library. It assumes that you have a
working Clojure environment; if not please see a
[getting started with clojure](getting-started-with-clojure.md) documentation.
It also assumes that you have a knowledge of OWL and ontologies in general; if
not please see the description of [what OWL is](what-is-owl.md).

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
for your purposes. It is also possible to use Tawny as an
[API](tawny-as-an-api.md) to maninpulate existing OWL, although it was not
designed for this purpose and has some limitations.

In this document, we will explore the basic usage of the library, using
examples from the tawny-pizza -- the traditional
[Pizza Ontology](http://robertdavidstevens.wordpress.com/2010/01/22/why-the-pizza-ontology-tutorial/).
A working version of this ontology ported to Tawny is
[available](https://github.com/phillord/tawny-pizza).


## Creating a new project

The most straight-forward to create a new project is to use leiningen. So

    lein new pizza

will create a new project in the directory pizza. This will contain a number
of files, including a `project.clj` file, a `src` directory and a `test`
directory. You need to add Tawny-OWL as a dependency to the project file as
described in [getting-started-with-clojure](getting-started-with-clojure.md).

## Creating a new ontology

Clojure has a namespacing mechanism which is used to prevent name clashes.
Although, it's not necessarily true, a clojure namespace generally maps to a
single file. Rather like Java, namespaces can also contain packages, which
map to directories. So, for example, the tawny-pizza ontology is defined in a
single namespace called `pizza.pizza`. This maps to a file in
`src/pizza/pizza.clj`. In this file, we find the namespace declaration:

    (ns pizza.pizza
      (:use [tawny.owl])
      (:require [tawny
                 [polyglot]
                 [reasoner :as r]
                 [pattern :as p]]))


As well as describing the namespace of the file (`pizza.pizza`), it also
contains a `(:use [tawny.owl])` form; this imports all of the functions in the
`tawny.owl` namespace, allowing them to be used without qualification; we also
require three other namespaces from Tawny-OWL, which must be qualifed in use,
and define short-cuts for these.

In normal usage, Tawny-OWL uses a single ontology for in a single namespace.
Before being used an ontology must be declared:

    (defontology pizzaontology
      :iri "http://www.ncl.ac.uk/pizza"
      :prefix "piz:"
      :comment "An example ontology modelled on the Pizza tutorial ontology from Manchester University,
    written using the tawny-owl library"
      :versioninfo "Unreleased Version")

This declares a new ontology, with the given IRI and prefix (which has no
formal semantics but will be used when the ontology is serialised).


## Creating a new class

Creating new classes happens in a similar manner to an ontology. The following
forms created three classes. In this case, we add a set of annotation
properties as well.

    (defclass Pizza
      :label "Pizza")


    (defclass PizzaTopping)

    ;; currently we have to use the annotation function with label to pass a
    ;; language in.
    (defclass PizzaBase
      ;; the pizza ontology contains some Portuguese labels. The :label keyword
      ;; used above is a shortcut for English
      :annotation (label "BaseDaPizza" "pt")))


Declaring a class in this manner adds it to the current ontology, as well as
making it available for use in later definitions. The `defclass` form is the
usual way to declare classes because it can use the frame-like syntax to add
arbitrary restrictions to the class, including superclasses, disjoints, and
equivalent classes. However, where no restrictions need to be added, or where
restrictions will be added later, `declare-classes` allows defining many
classes at once. For example, four classes can be defined at once:

    (declare-classes ChickenTopping
                   HamTopping
                   HotSpicedBeefTopping
                   PeperoniSausageTopping)

## Creating properties

Properties can be created in a similar manner to classes, using the
`defoproperty` form. This also has a frame like syntax; a relatively complex
example, demonstrates several of these frames:

    (defoproperty hasBase
      :subpropertyof hasIngredient
      :characteristics functional
      :range PizzaBase
      :domain Pizza
      )

This requires that the various objects, such as `hasIngredient`, `Pizza` and
`PizzaBase` already have been created; `functional` is defined by Tawny-OWL
itself.

Annotation properties can be created similarly with `defannotationproperty`.
Currently, creating datatype properties requires direct use of the OWL API;
extension of Tawny-OWL to cope is planned, pull requests gratefully accepted.

## Disjoints, Inverse and subclasses

The full use of restrictions is covered [later](adding-restrictions.md),
however Tawny-OWL provides some easy to use macros which enable adding common
restrictions; for example, the `as-inverse` macro makes object properties
defined within it as inverses. For example, `hasIngredient` and
`isIngredientOf` are declared as inverse here:

    (as-inverse
     (defoproperty hasIngredient
       :characteristics transitive)
     (defoproperty isIngredientOf
       :characteristics transitive
       ))

Likewise, it is possible to add a shared superclass and disjoint statements to
every class. For example, the following combines the `declare-classes` and
`as-disjoint-subclass` macro.


    (defclass CheeseTopping)

    (as-disjoint-subclasses
     CheeseTopping

     (declare-classes
      GoatsCheeseTopping
      GorgonzolaTopping
      MozzarellaTopping
      ParmesanTopping))


There is also a `as-subclasses` macro which takes a number of parameters. For
example, consider this definition (not from the pizza ontology) which defines
the primary colours.

    (defclass PrimaryColour)

    (as-subclasses
        PrimaryColour
        :disjoint :cover
        (declare-classes Red Green Blue)
    )


In this case, each of `Red`, `Green` and `Blue` are declared as being disjoint
from each other, and further as "covering" `PrimaryColour`. This would
generate the following ontology, represented in Manchester syntax:

    Class: PrimaryColour
        EquivalentTo:
            Red or Green or Blue

    Class: Red
        SubClassOf: PrimaryColour

    Class: Green
        SubClassOf: PrimaryColour

    Class: Blue
        SubClassOf: PrimaryColour

    DisjointClasses:
        Red, Green, Blue


Tawny-OWL can be considerably more succient than Manchester OWL.

## Adding and Removing entities interactively.

As Tawny-OWL is based on Clojure, it is possible to use Tawny-OWL
interactively, through a REPL (Read-Eval-Print-Loop). In this environment, it
is possible to add and, also, to remove entities interactively. This can be
useful for trying out ideas. The `defclass` and other macros shown far work
perfectly well in this environment. It is also possible to remove statements
that have been added. For example, consider the following statement:

    (defclass A)
    (defclass B)
    (remove-entity B)

After the last form, the ontology will only contain the class `A`.

## Saving Ontologies

Saving an ontology is simple, using the `save-ontology` form. This takes a
filename and potentially a format.

    (save-ontology "ontology.omn" :omn)
    (save-ontology "ontology.owl" :owl)

The file location is relative to the working directory of the current REPL; in
practice, this means in the same directory as the `project.clj` file, rather
than the directory of the file that contains the `save-ontology` form.


## Visualising

[Protege](http://protege.stanford.edu/) provides an excellent environment for
visualisating and navigating through an ontology. It interacts quite nicely
with Tawny-OWL, as a viewer rather like using a Web Browser while developing a
web site, or a PDF viewer while writing LaTeX.

The best way to achieve this is with Tawny-OWL, is to add a `save-ontology`
form to the end of each namespace containing an ontology, or alternatively add
a main method that saves all ontologies in a project. For this use, the `:owl`
or `:rdf` form work best. Protege can open files and view them; it also copes
quite well with having files changes underneath it, and offers to revert to
the saved version.

A similar effect can be achieved with Emacs and omn-mode which provides syntax
highlighting for Manchester syntax. In this case, `auto-revert-mode` works to
handle updating.


## Next

 - [Adding Restrictions](adding-restrictions.md)
 - [Reasoning](reasoning.md)
