Tawny-OWL
===========

[![Build Status](https://travis-ci.org/phillord/tawny-owl.png?branch=master)](https://travis-ci.org/phillord/tawny-owl)

## Introduction

This package allows users to construct OWL ontologies in a fully programmatic
environment, namely Clojure. This means the user can take advantage of
programmatic language to automate and abstract the ontology over the
development process; also, rather than requiring the creation of ontology
specific development environments, a normal programming IDE can be used;
finally, a human readable text format means that we can integrate with the
standard tooling for versioning and distributed development. A longer
[getting started](docs/getting-started.md) document is available.

OWL is a W3C standard ontology representation language; an ontology is a fully
computable set of statements, describing the things and their relationships.
They are used, mostly notable in biomedicine, to describe complex areas of
knowledge such as [genetics](http://www.geneontology.org/) or
[clinical terminology](http://en.wikipedia.org/wiki/SNOMED_CT), but can
describe anything, including [e-commerce](http://purl.org/goodrelations/). For
more tutorial information, please see http://ontogenesis.knowledgeblog.org.

The core library, `owl.clj` provides an API which looks similar to Manchester
OWL syntax, but is nonetheless fully programmatically extensible; all
namespaces are [described](docs/namespaces.md) elsewhere.

## Motivation

I discuss the development of this code base in my
[journal](http://www.russet.org.uk/blog). Two recent posts include one on the
[motivation] (http://www.russet.org.uk/blog/2214) and another on making the
library more ["lispy"](http://www.russet.org.uk/blog/2254). All revelevant
posts are
[cateogorised](http://www.russet.org.uk/blog/category/all/professional/tech/tawny-owl).

## Installation

Tawny-OWL requires no installation *per se* and is used as any Clojure
library. It is available from
[clojars](https://clojars.org/uk.org.russet/tawny-owl), so just add:

`[uk.org.russet/tawny-owl]' to your `project.clj' file. 

I use Leiningen 2 on the current version 12.04 Ubuntu and, occasionally, on
Windows. Editing of both tawny-owl and the ontologies using it, is with Emacs
24 using Clojure mode and nrepl, currently both installed from their
respective versioning systems. The library should not depend on this
environment, however.

## Author

Phillip Lord, Newcastle University.

http://www.russet.org.uk/blog

## Mailing List

There is a [mailing list](mailto:tawny-owl@googlegroups.com)

## License

The contents of this file are subject to the LGPL License, Version 3.0.

Copyright (C) 2012, 2013, Newcastle University

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see http://www.gnu.org/licenses/.
