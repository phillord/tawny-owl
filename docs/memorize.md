Memorizing Read Ontologies
==========================

Memorization allows tawny to cope with change in ontology dependencies, by
tracking the identifiers that tawny has used in the past, so that it can
recognise changes in the future. This is important for [reading](importing)
ontologies which use hard-to-remember identifiers.


## The problem

The problem that memorize addresses was first described in an
[article](http://www.russet.org.uk/blog/2316). Many ontologies use numeric
identifiers; this are hard to use computationally, because they are hard for
the developer to remember. Tawny can provide an arbitrary transformation of
OWL annotation properties to produce a readable label. However, downstream
ontologies may change these labels which will break any ontologies build using
these.

In a GUI environment, this causes less problems because there is a separation
of the underlying model and what the user sees; if the label changes, and the
identifier does not, a GUI will just show the user the update. This is less
easy to achieve with a code base, but tawny manages this with `memorise.clj`.

## Solution

The memorization functionality stores the mappings between label and
identifiers when ever a downstream OWL file is read. When a file is read for a
second time, Tawny checks these mappings to see if any of the identifiers has
a new label. In this case, tawny effectively generates an alias to the entity
using both the old and new label. However, use of the old label will result in
a deprecation warning, printed to the repl. This means that downstream
ontologies will still work, while allowing developers to migrate to new
identifiers.

## In Practice

Use of memorise is relatively straight-forward. The upstream ontology needs to
be read as described [previously](importing). To save the mappings that have
been made, use the `memorize` function:

    (tawny.memorise/memorise obi "./src/tawny/obi_memorise.clj")

The location for the "memorise" is variable, but I use the same directory as
the load file because it will be versioned along with other source. If you are
using a downloaded version of the OWL source, this function only needs to be
run every time it is updated.

To remember the mappings made previously, we use the counter-part `remember`
function.

    (tawny.memorise/remember obi "./src/tawny/obi/obi_memorise.clj")

Memorise generates a print statement for all the obsolete mappings that it
finds, and will generate another print statement whenever the obsolete term is
actually used.


## Next

 - [OBO style, numeric identifiers](obo.md)
