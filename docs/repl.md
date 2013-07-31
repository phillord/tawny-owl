Use at the REPL
===============

Clojure is an interactive language, in that it provides a REPL --
read-eval-print-loop; what this means is that you can write code
interactively, adding and removing functions, to test them out. This turns out
to be a surprisingly useful feature of the language. This was one of the
original motiviations for using Clojure, as adding and removing concepts
without the need for a long compile-restart cycle seemed a critical feature.
Here we describe use of the REPL and some features of tawny that aid it's use.

## Starting the REPL

How to start the REPL is entirely dependent on the development environment
that you are using. For Emacs, the command "M-x nrepl-jack-in" achieves this;
for other environments, please read the documentation (it is likely to be near
the beginning, since most Clojure developers use the REPL a lot).

Typically, the REPL will have a prompt such as:

    user>

This is not shown in this document, as it prevents cutting-and-pasting. The
REPL also prints the return value of every form. We show this here preceded
with a comment character, again, to enable cutting-and-pasting.

## Creating an ontology

To use the REPL directly, you need to `require` or `use` the `tawny.owl`
library. In the default namespace, type:

    (use 'tawny.owl)
    ;;nil

We can now create a new ontology. This can be done with the standard
`defontology` form. As it is so useful, there is also an `test-ontology`
function which can be used to create a "disposable" ontology.

    (test-ontology)
    ;; #<OWLOntologyImpl Ontology(OntologyID(OntologyIRI(<http://iri/>))) [Axioms: 0 Logical Axioms: 0]>


## Adding and Removing entities

Entities such as classes and properties can be added, using the same commands
as normal. For example, we can create a new class called `A`.

    (defclass A)
    ;; #'user/A

The return value here, is Clojure var object created by the `defclass` form;
the reasons for this is consistency with Clojure which does this with all
`def` form; the reasons for Clojure doing this are a little obscure and not
described here. To see the `OWLObject` created, type the variable name again.

    A
    ;; #<OWLClassImpl <http://iri/#A>>

Use of the functional forms will return this value directly.

    (owlclass "A")
    ;; #<OWLClassImpl <http://iri/#A>>

Because of the behaviour of the OWL API, calling this function twice will
result in an equivalent object.

    (= A (owlclass "A"))
    ;; true
    (= (owlclass "A") (owlclass "A"))
    ;; true
    (= (owlclass "A") (owlclass "B"))
    ;; false

These functions do have the side effect of adding entities to the ontology.
So, for example, after typing the forms above, we find:

    (get-current-ontology)
    ;; #<OWLOntologyImpl Ontology(OntologyID(OntologyIRI(<http://iri/>)))
    ;; [Axioms: 2 Logical Axioms: 0]>

the current ontology has 2 axioms in total -- we have created (owlclass "B").
If we wish to remove the entity again, we can do so. Note that this does not
delete the class or remove it from the namespace, just from the ontology.

    (defclass B)
    ;; #'user/B
    B
    ;; #<OWLClassImpl <http://iri/#B>>
    (get-current-ontology)
    ;; #<OWLOntologyImpl Ontology(OntologyID(OntologyIRI(<http://iri/>)))
    ;; [Axioms: 2 Logical Axioms: 0]>
    (remove-entity B)
    ;; [#<RemoveAxiom RemoveAxiom(Declaration(Class(<http://iri/#B>))
    ;;   OntologyID(OntologyID(OntologyIRI(<http://iri/>))))>]
    B
    ;; #<OWLClassImpl <http://iri/#B>>
    (get-current-ontology)
    ;; #<OWLOntologyImpl Ontology(OntologyID(OntologyIRI(<http://iri/>)))
    ;; [Axioms: 1 Logical Axioms: 0]>

## Repl Facilities

Tawny also provides a documentation lookup facility rather like the native
Clojure facility, although it operates in a different way. To use this, first
the library must be required.

    (require 'tawny.repl)
    ;; nil

We can now check for the documentation. For example:

    (tawny.repl/print-doc B)
    ;; Prints out
    ;; Class: B
    ;; IRI: http://iri/#B
    ;; Labels:
    ;; Comments:
    ;; Full Definition:
    ;; (tawny.owl/defclass http://iri/#B)
    ;;
    ;; Returns value!
    ;; nil

In this case, the documentation string is not that useful, so let's add more
to the definition of `B`.

    (defoproperty P)
    ;; #<OWLObjectPropertyImpl <http://iri/#P>>
    (defclass B :label "B" :comment "B is a test class" :subclass (owlsome P B))
    ;; #'user/B
    (tawny.repl/print-doc B)
    ;; Prints out!
    ;;
    ;; Class: B
    ;; IRI: http://iri/#B
    ;; Labels:
    ;; 	"B"@en
    ;; Comments:
    ;; 	"B is a test class"@en
    ;; Full Definition:
    ;; (tawny.owl/defclass
    ;;  http://iri/#B
    ;;  :subclass
    ;;  (owlsome http://iri/#P http://iri/#B)
    ;;  :annotation
    ;;  (owlcomment "B is a test class" "en")
    ;;  (label "B" "en"))
    ;; Return value!
    ;; nil

As can be see, comments and labels are printed, as is a "full definition".
This is calculated from the object itself, so will work even with definitions
that are [read](importing.md#reading) from an OWL file.

## Native Clojure documentation

Tawny can place documentation in the same place as Clojure normally places it;
this means that documentation lookup will probably work as with normal
functions.

Unfortunately, this is not totally straight-forward; the documentation lookup
system of Clojure makes the assumption that Vars are immutable; unfortunately
with tawny, the vars point to mutable Java objects. The documentation may
change as the ontology is built.

It is possible to update the documentation on the Clojure var to represent the
current state of the ontology using `(tawny.repl/update-doc MyClass)`, or
alternatively, to update all the Var in a ns using `(tawny.repl/update-ns-doc
*ns*)` or `(tawny.repl/update-ns-doc (find-ns 'my-namespace))`.
