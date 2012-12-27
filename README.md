Tawny-OWL
===========
:author: Phillip Lord


== Introduction

This package uses the clojure and the OWL API to construct OWL ontologies. 

At the moment it is in its very early stages and really does not do much,
although you could use it to build an ontology. The file +owl.clj+ contains
the library (to be!), while +core.clj+ contains some sample test code. At the
moment, OPPL is a more complete solution. However, I want this for a number of
other reasons: I would like to be able to interact with a reasoner in batch;
and I want to be able to build more formal test cases over an ontology. I
should be able to build all of this, just piggy-backing on the clojure
environment. 

This is the first and only piece of code that I have written in clojure; it no
doubt shows this heritage. 

I discuss the development of this code base in my
[journal](http://www.russet.org.uk/blog). Two recent posts include one on the
[motivation] (http://www.russet.org.uk/blog/2214) and another on making the
library more ["lispy"](http://www.russet.org.uk/blog/2254). All revelevant
posts are
[cateogorised](http://www.russet.org.uk/blog/category/all/professional/tech/tawny-owl).

This package was previously named "Clojure-OWL".

== Installation

We are at too early a stage to require an installation. The project builds
with leiningen 1.7, which is the current version on Ubuntu. I am using Emacs
23 with clojure mode to do all the interaction, installed from the Marmalade
repository. 

== Author

Phillip Lord, Newcastle University. 
http://www.russet.org.uk/blog


== Licence

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


