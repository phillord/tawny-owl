Reasoning
=========


## Introduction

One of the key features of OWL, is that it has a well-defined and formal
semantics; a practical upshot of this is that it is possible to implement a
reasoner which additional conclusions from the statements defined in the
ontology.

Tawny provides easy access to reasoning, both for consistency checking and for
querying of the ontology.

[Ontogenesis](http://ontogenesis.knowledgeblog.org) has a number of articles
which help to describe these concepts used here:
 - The meaning of satisfiable, coherent and inconsistent
   (http://ontogenesis.knowledgeblog.org/1329)
 - OWL2 and it's semantics (http://ontogenesis.knowledgeblog.org/55)
 - Classes and their extents (http://ontogenesis.knowledgeblog.org/1004)


## A reasoning task

While we cannot hope to give a full introduction to reasoning here, we use a
simple example to demonstrate, taken as usual from the
[tawny-pizza](https://github.com/phillord/tawny-pizza) ontology.

Here we introduce a *defined* class. The `equivalent` frame describes the
necessary and *sufficient* conditions for membership of this class. Any class
which is both a `Pizza` and have a `CheeseTopping` will be considered to be a
`CheesyPizza` whether this is explicitly stated or not.

    (defclass CheesyPizza
      :equivalent
      (owland Pizza
               (owlsome hasTopping CheeseTopping)))

We can also introduce non-sensical classes which cannot make sense under any
circumstances. Here we introduce the simplest example:

    (defclass A
        :subclass B (owlnot B))

This class never has any classes.

## Selecting a reasoner

Before starting, a reasoner needs to be selected. In principle, any reasoner
which supports the OWL API could be used. Practically, however, this also
requires that they be "mavenized": currently, this limitation means that only
my mavenized version of HermiT (http://github.com/phillord/hermit-maven) and
ELK (http://code.google.com/p/elk-reasoner/).

To use the reasoning package, the `tawny.reasoner` must be `require`'d, first.
Then selecting the relevant reasoner is straight-forward.

    (require 'tawny.reasoner)
    (tawny.reasoner/reasoner-factory :hermit)

In the pizza ontology, we `require` in the `use` form and set up and alias. We
follow this convention in the rest of this manual.

    (ns pizza.pizza
      (:use [tawny.owl])
      (:require [tawny
                 [read]
                 [polyglot]
                 [reasoner :as r]
                 [pattern :as p]]))

    (r/reasoner-factory :elk)

## Coherency

An ontology is coherent if all the concepts could, under some circumstances
have an individual. The simplest example of an incoherent ontology is one
containing the class `A` above; this class can never have individuals since it
is not possible to be both `B` and `not B` at the same time. To check for
coherency, we use the `coherent?` predicate

    (r/coherent?)

This will return false if the ontology contains any classes such as `A` which
cannot have an individual. At the time of writing, this function only works
over the current ontology, which can be changed with the `with-ontology`
macro. This method also attempts to generate a GUI progress-bar; see later for
who to change this behaviour.

## Unsatisfiable

A class which can never have any individuals is called `unsatisifiable` as
the restrictions placed upon it can never be fulfiled or satisfied. To find
the unsatisifiable classes, simply call;

    (r/unsatisfiable)

This returns a set of unsatisifiable classes.

## Consistency

An ontology is inconsistent if it contains any unsatisifiable classes which
are, none-the-less asserted to have individuals. To check for this, simply
call;

    (r/consistent?)


## Class Relationships

It is also possible to infer class relationships, using a computational
reasoner. For instance:

    (defclass MargheritaPizza
        :subclass Pizza
            (someonly hasTopping MozzarellaTopping TomatoTopping))


will be infered to be a subclass of `CheesyPizza` because it has a
`MozzarellaTopping` which is, in turn, a `CheeseTopping`. These class
relationships are *not* returned from the normal `tawny.owl` functions, such
as `subclasses` nor their equivalent predicates `subclass?`. For these, the
`tawny.reasoner` functions, for example `isubclasses`, `isubclass?` must be
used instead. In all cases, these return sets which can be operated over as
normal clojure sets.


## tawny-mode.el

For those using Emacs, tawny-mode.el comes with integrated support for
reasoning. To enable it, add

    (require 'tawny-mode)
    (add-hook 'clojure-mode-hook 'tawny-mode-maybe-enable)

to your `.emacs`. This function checks for some of the more common macros in
`tawny.owl`, rather than turning on reasoning in all clojure buffers where it
mostly does not make sense. This is a little cluny, but does not require a
live nrepl buffer to operate.

## Reasoning GUIs

By default the reasoner class pops up a rather cheesy progress-bar when
functional. This is useful, but can be irritating, particular when attempting
to use tawny in headless mode, for instance, when unit testing. This behaviour
can be changed by rebinding `tawny.reasoner/*reasoner-progress-monitor*`. For
example, the following code is taken from the test fixture for `tawny.pizza`.

    (defn ontology-reasoner-fixture [tests]
      ;; this should kill the reasoner factory and all reasoners which is the
      ;; safest, but slowest way to start.
      (r/reasoner-factory :hermit)
      ;; inject the pizzaontology into the current namespace, which saves the
      ;; hassle of using with ontology every where. set this up each time in case
      ;; pizzaontology has been re-evaled
      (o/ontology-to-namespace p/pizzaontology)
      (binding [r/*reasoner-progress-monitor*
                (atom r/reasoner-progress-monitor-silent)]
        (tests)))


It is also possible to change the default using the `reset!` function, which
will change the default; this is most useful at the REPL, or potentially in a
`run` method.

    (reset! r/*reasoner-progress-monitor* r/reasoner-progress-monitor-silent)

Currently, three GUIs are provided:
`tawny.reasoner/reasoner-progress-monitor-gui`,
`tawny.reasoner/reasoner-progress-monitor-text` and
`tawny.reasoner/reasoner-progress-monitor-silent`, which use a swing GUI,
text, or are totally silent. Note that if you use the default, GUI class, it
will prevent the Java virtual machine terminating after, for example, running
`lein run`.



## Next

 - [Querying the ontology](querying.md)
