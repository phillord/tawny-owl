Testing
=======

Clojure comes with support for unit testing and it possible to use this
framework to unit test ontologies, both with and without reasoning. This
allows automated checking both of expected inheritance patterns, and that
inferencing is working as expected.

In this document, we use `clojure.test` which is one of a number of test
frameworks, because it is already available  in Clojure.

## A basic test

Tests in Clojure are rather like Java -- they are generally stored in a
directory structure which mirrors the `src` tree. In clojure tests use a file
(and namespace) with `-test` appended. A simple test structure looks like so:


    (ns my.ontology-test
       (:use [clojure.test])

    (deftest test-name
      (is
        true)
      (is
        (not false)))

The `test-name` is used as a label in any error messages; the `is` form
asserts that something is true, that is returns `true` (for Clojure's
definition of `true` -- this is not the same as either Java or many Common
Lisp!). In general, the `is` forms should be highly related tests; otherwise,
multiple `deftest` forms are more appropriate.

## Fixtures

`clojure.test` provides support for "fixtures" -- functions that define the
environment within which this tests run. This turns out to be very useful for
tests in `tawny`; generally, these are used to set the reasoner appropriately,
to set the reasoner progress monitor, and set the ontology to be tested to as
the current ontology for the test namespace. The following code from the pizza
ontology achieves this.

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

    (use-fixtures :once ontology-reasoner-fixture)

## Consistency

The art of testing ontologies is not well-developed at the current time, so
the main use for testing is checking for
[coherence](http://ontogenesis.knowledgeblog.org/1329), and the specific
consequences of reasoning. Both of these are straight-forward.

    (deftest consistency
      (is (coherent?))
      (is (consistent?)))

This test can be useful, including for ontologies written externally to OWL
and [read](importing.md) into tawny; tawny can be used to define unit tests
against any OWL ontology whether written in tawny or not.

## Inference

A second significant usage for testing is to determine whether specific
interferences has taken place. The `tawny.reasoner` namespace provides a
number of tests for reasoned superclasses which allows testing for this. This
example from the pizza ontology demonstrates this usage.


    (deftest CheesyShort
      (is (r/isuperclass? p/FourCheesePizza p/CheesyPizza))
      (is
       (not (r/isuperclass? p/MargheritaPizza p/FourCheesePizza))))


## Checking for disjointness

A second use is to check for inferred disjointness; this is not done directly,
but by introducing a subclass of the two expected classes, and checking for
incoherence. A class introduced in this way is described as a "probe class"
`tawny` provides explicit support for these classes, taking care of removing
them again afterwards. This example, for instance, tests whether our
definition of vegetarian pizza is behaving as expected.

    (deftest VegetarianPizza
      (is
       (r/isuperclass? p/MargheritaPizza p/VegetarianPizza))
      (is
       (not
        (o/with-probe-entities
          [c (o/owlclass "probe"
                         :subclass p/VegetarianPizza p/CajunPizza)]
          (r/coherent?)))))

The expected inference is checked in the first `is` clause, while the second
`is` clause checks that the ontology is now incoherent.
