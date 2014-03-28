Querying
========


## Introduction

It is possible to query an ontology, to navigate parent and child
relationships in between classes. Here we describe how to query over just the
asserted relationships: [reasoning](reasoning.md) is handled independently. 

## In use

Given classes such as:

    (defclass A)
    (defclass B
        :super A)
    (defclass C
        :equivalent B)

We can query classes for these different relationships. Return values are
shown as comments.

    (subclasses A)
    ;; (#<OWLClassImpl <http://iri/#B>>)

Note that this *does not* return `C`, which might be expected; logically, as
`C` is equivalent to `B`, and `B` is a subclass of `A` then it is true that
`C` is a subclass of `A`. However, the OWL API cannot determine this without
the use of interferencing, for which we must use `tawny.reasoner`.

Tawny also provides a number of predicates for testing relationships.

    (equivalent? B C)
    ;; true
    (superclass? B A)
    ;; true
    (subclass? A B)
    ;; true

Again, these options only work over asserted relationships. Hence:

    (superclass? B C)
    ;; false
    (subclass? B C)
    ;; false

## Next

 - [Reasoning](reasoning.md)
