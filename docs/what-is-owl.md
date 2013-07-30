What is OWL
===========

This is a general introduction to ontologies and OWL. There are a number of
general resources available that also cover these same topic, notably
[ontogenesis](http://ontogenesis.knowledgeblog.org/), so this introduction is
fairly brief.

## What is an ontology?

The struggle with defining what an ontology is has been fairly long and
traumatic; here, we define it as a representation of knowledge in a domain. In
a sense, this is true of any data model, so more specifically, ontologies
normally consist of a set of concepts with properties or relationships between
them; most ontologies are either frame based, that is have a set of defined
slots into which relationships fit, or have use a description logic -- a form
of logic built for the purpose.

For a longer description please see the article on
[ontogenesis](http://ontogenesis.knowledgeblog.org/66).

## Why use an ontology?

Ontologies are most useful when a complex domain needs to be represented
computationally; see [ontogenesis](http://ontogenesis.knowledgeblog.org/1296)
for a more detailed description.

In general, the knowledge needs to be fairly complex to make use of an
ontology worthwhile; ontologies largely describe things categorically, so are
not useful, for instance, where there is a lot of probabalistic or numerical
data; this is where a statistical model will work better.

To a programmer, an ontology looks somewhat like a type system, although
ontology languages are generally more expressive, but do not interact with an
underlying programming language.

Probably the most heavy use of ontologies has been in biomedicine, where very
large ontologies have been used to annotate
[data](http://ontogenesis.knowledgeblog.org/50) data.

## What is OWL?

[OWL](http://ontogenesis.knowledgeblog.org/55) is a W3C
[standard](http://www.w3.org/TR/owl2-overview/) language that can be used to
build ontologies. OWL has two main selling points; firstly, as a W3C standard,
it integrates with all the other W3C standard, builds on
[RDF](http://ontogenesis.knowledgeblog.org/235) and XML and is fully web
capable; and, second, it maps to a well defined description logic, the
practical upshot of which is that it is possible to perform computational
reasoning over OWL statements, draw inferences and detect inconsistencies.

## Further Resources

The
[OWL pizza tutorial](http://owl.cs.manchester.ac.uk/tutorials/protegeowltutorial/)
provides a good overview for building ontologies in OWL, although it uses
Protege rather than tawny.

The [Good OD tutorial](http://purl.org/goodod/guideline) provides a good
description of both the philosophical and comptational issues of ontology
building.

[tawny-pizza](https://github.com/phillord/tawny-pizza) is a port of the Pizza
ontology to use tawny and provides a nice example ontology.

[tawny-simple](https://github.com/phillord/tawny-simple) is a
literately-programmed use of tawny describing the inferences that can be made
on the basic of OWL statements.
