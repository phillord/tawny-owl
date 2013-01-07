Tawny-OWL
===========

## Introduction

This package uses the clojure and the OWL API to construct OWL ontologies. 

The core library, `owl.clj` provides an API which looks similar to Manchester
OWL syntax, but is nonetheless fully programmatically extensible. Other names
spaces include: 

* `reasoner.clj` -- provides access to reasoning services. Currently supports
  either [ELK](http://code.google.com/p/elk-reasoner/) or
  [HermiT](http://hermit-reasoner.com/).
* `read.clj` -- loads a OWL file into the current clojure namespace, so that
  it can be built against. 
* `pattern.clj` -- start of a ontology pattern library. 
* `render.clj` -- render an OWL class into text or a lisp form. 



I discuss the development of this code base in my
[journal](http://www.russet.org.uk/blog). Two recent posts include one on the
[motivation] (http://www.russet.org.uk/blog/2214) and another on making the
library more ["lispy"](http://www.russet.org.uk/blog/2254). All revelevant
posts are
[cateogorised](http://www.russet.org.uk/blog/category/all/professional/tech/tawny-owl).

This package was previously named "Clojure-OWL".

## Installation

We are at too early a stage to require an installation. The project builds
with leiningen 1.7, which is the current version on Ubuntu. I am using Emacs
23 with clojure mode to do all the interaction, installed from the Marmalade
repository. 

## Author

Phillip Lord, Newcastle University. 
http://www.russet.org.uk/blog


## License

The contents of this file are subject to the LGPL License, Version 3.0.

Copyright (C) 2012, Newcastle University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.


