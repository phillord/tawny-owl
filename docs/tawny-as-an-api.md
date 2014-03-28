Tawny as an API
===============


Although, it was originally intended as a tool for developing ontologies,
Tawny can work reasonably effectively as a general purpose API. However, it is
not a "normal" clojure API; this document seeks to highlight the differences,
as well as giving a broad overview of the API. It assumes a background
knowledge of OWL and Clojure.

Global Concerns
---------------

Tawny is also a wrapper for the OWL API, using a Lisp because it has almost no
syntax, so is highly manipulable, and because it allows evaluation. Clojure
was chosen partly because of the nice Java integration, and partly just
because it was. It is not really a clojure library, it's a text based user
interface for ontology development (written in clojure).

Tawny wraps the OWL API fairly straight-forwardly. This means that the vast
majority of functions are not pure, change state, and are not thread safe. I
have not followed Clojure conventions in add `!` to these functions as it is
most of them and it would be extra baggage.


Frames based functions
---------------------

As an API, tawny has quite a few entry points, but the most straight-forward
to use are the functions `ontology`, `owl-class`, `individual`,
`object-property`, `datatype-property` and `datatype`. These also have
equivalent macros which define vars at the same time. They all have a similar
form taking keyword arguments, although again this is specialised so that each
keyword can take multiple values.

    (owl-class "a" :super "b" "c" "d")

In most cases, it is possible to use either strings or existing OWL API
objects; however, it must be possible for tawny to differentiate between the
different OWL constructs. For instance, in a new namespace:

    (ns new)
    (defontology a)

    ;; will crash, we do not know whether "r" is an object or data property
    (owl-some "r" "b")

    ;; add b to the ontology
    (owl-class "b")

    ;; will succeed because now we know what "b" is, which means that "r" must
    ;; be an object property.
    (owl-some "r" "b")

This also highlights a second feature of tawny. The `defontology` macro
creates an ontology, stores it in a var (called `a`) and makes this the
default ontology (of which there can be one per namespace).

While this is extremely convienient when using tawny to develop an ontology,
it's fairly ugly for manipulating one; particularly because it it makes using
several ontologies at once difficult. In the next section, we discuss how to
avoid this problem.

The ontology argument
---------------------

It is possible to set the "current ontology" using dynamic scoping and the
`with-ontology` macro, using what has been called the
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

In addition to this default argument, the main frame based macros ---
`defclass`, `defindividual`, `defoproperty`, `defdproperty` and `defdatatype`
--- all accept an `:ontology` argument, where a first argument doesn't make
sense. The keyword can also be used with the function equivalents.

Broadcasting
------------

Most of the functions in Tawny "broadcast". That is they take any number of
arguments, and apply a fixed number (usually two) against all of the rest --
confusing to explain, hopefully easy to use.

Consider, for instance the `add-label` function which takes two fixed
arguments. (The add-* functions are described more fully in the next section).

    ;; the most straight-forward usage -- add the label "label1" to clazz in
    ;; the given ontology
    (add-label ont clazz "label1")

    ;; the same using the default ontology
    (add-label clazz "label1")


However, this function takes an arbitrary number of labels.

    (add-label ont clazz "label1" "label2" "label3" "label4")

More over, it takes them in an arbitrary structure, and will flatten them.

    (add-label ont clazz "label1" ["label2" ["label3" ["label4"]]])

which does the same.

The frame-based functions do this for each of their frames. So, equivalently
to the last few examples.

    (owl-class clazz :label "label1" "label2" "label3" "label4")
    ;; or
    (owl-class clazz :label "label1" ["label2" ["label3" ["label4"]]])

This provides considerable flexibility to the library. For example, this has
been used with the `some-only` function to enable
[closure](http://ontogenesis.knowledgeblog.org/1001) (not Clojure!) axioms.
The `some-only` function returns several restrictions at once, as a list.

    (owl-class "class" :super (some-only "r" "a" "b" "c"))
    ;; this is the same as...
    (owl-class "class" :super
        (owl-some "r" "a")
        (owl-some "r" "b")
        (owl-some "r" "c")
        (only "r" (owl-or "a" "b" "c")))

As the `owl-some` function also broadcasts, this can also be written more
simply as:

    (owl-class "class" :super
        (owl-some "r" "a" "b" "c")
        (only "r" (owl-or "a" "b" "c")))

In addition to this flexibility, the frame based functions uses specialised
support for keyword arguments rather than Clojure destructuring, which means
the same keyword can be supplied multiple times. So, equivalent to the
examples given earlier, four labels could also be given like this:

    (owl-class clazz :label "label1" :label "label2"
                     :label "label3" :label "label4")

This makes it easy to write specialised extensions of these functions.
Consider this silly example:

    (defn silly-class [& args]
            (apply owl-class (concat args [:comment "This class is silly"])))
    (silly-class "c" :comment "Although it's not my fault")

The `silly-class` function will handle frames exactly the same as `owl-class`,
rather than following a "last keyword wins" semantics, which would remove any
comments provided to the `silly-class` call.

The `add-*` functions
---------------------

As well as the main "frame" functions, tawny uses a large number of `add`
functions -- internally, these are used to consume one of the frames each.
These are lower-level than the frame functions and come with a little less
baggage, so probably run faster. In some cases, however, they require OWL API
objects and not strings.

    ;; these two are equivalent
    (owl-class "a" :comment "This is a comment")
    (add-comment (owl-class "a") "This is a comment")

    ;; and so are these (in both ways!). add-equivalent can take strings.
    (owl-class "a" :equivalent "b") (add-equivalent "a" "b")

Although these are mostly public, this may change in future. The frame
functions are designed as the main entry point.

The `*-explicit` functions
--------------------------

The frame functions do a fair amount of checking and some cleverness to
produce what is hopefully a good interface for the ontology developer.
However, this comes at a computational and convienience cost for the use as an
API. For instance, they will complain if an unknown frame is passed, and
combining the keywords requires several passes through the arguments.

Tawny provides a set of `*-explicit` functions which bypass all of this, but
expect the frames as a pre-organised map. So,

    ;; these two are equvialent
    (owl-class "a" :label "a label")
    (owl-class-explicit "a" {:label "a label"})

    ;; as are these
    (owl-class "b" :label "label1" "label2")
    (owl-class-explicit "b" {:label ["label1" "label2"]})


Conclusions
-----------

Tawny is not a conventional Clojure API. It was originally intended as a tool
for the ontology developer who wished to be able to programmatically extend
ontology development, rather than a programmatic tool for ontology
manipulation. The intention is always to follow the former, while hopefully
enabling the latter if possible.
