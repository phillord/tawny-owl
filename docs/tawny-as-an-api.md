Tawny as an API
===============


Although, it was originally intended as a tool for developing ontologies,
Tawny can work reasonably effectively as a general purpose API. It is,
however, effected by its original purpose. In particular, it wraps the OWL API
fairly straight-forwardly. This means that the vast majority of functions are
not pure, change state, and are not thread safe. I have not followed Clojure
conventions in add `!` to these functions as it is most of them.

As an API, tawny has quite a few entry points, but the most straight-forward
to use are the functions `ontology`, `owlclass`, `individual`, `objectproperty`,
`datatypeproperty` and `datatype`. These also have equivalent macros which
define vars at the same time. They all have a similar form taking keyword
arguments, although again this is specialised so that each keyword can take
multiple values.

    (owlclass "a" :subclass "b" "c" "d")

In most cases, it is possible to use either strings or existing OWL API
objects; however, it must be possible for tawny to differentiate between the
different OWL constructs. For instance, in a new namespace:

    (ns new)
    (defontology a)

    ;; will crash, we do not know whether "r" is an object or data property
    (owlsome "r" "b")

    ;; add b to the ontology
    (owlclass "b")

    ;; will succeed because now we know what "b" is.
    (owlsome "r" "b")

This also highlights a second feature of tawny. The `defontology` macro
creates an ontology, stores it in a var (called `a`) and makes this the
default ontology (of which there can be one per namespace).

While this is extremely convienient when using tawny to develop an ontology,
it's fairly ugly for manipulating one; particularly because it it makes using
several ontologies at once difficult. It is possible to set the "current
ontology" using dynamic scoping and the `with-ontology` macro, using what has
been called the
http://stuartsierra.com/2013/03/29/perils-of-dynamic-scope[top anti-pattern]
for Clojure by Stuart Sierra; most of his reasons are not relevant here,
because the use of the OWL API already means that dispatching to other threads
or lazy sequences are already problematic and best avoided. It does make using
several ontologies at once unwieldy however. For this reason, tawny also
supports an explicit optional first argument of an OWLOntology object for most
functions. If you agree with Stuart Sierra, then never use `defontology`, but
do `(def x (ontology :iri "http://example"))`. In this way the default
ontology will never be used. You can use multiple ontologies in a single
namespace without worrying that the wrong one will be used. 
