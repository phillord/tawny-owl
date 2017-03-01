Adding Restrictions
===================


## Frames

Tawny-OWL has a fairly flexible syntax, and provides a number of mechanisms
for specifying restrictions to classes and properties, including the macros
covered in [getting started](getting-started.md). The most basic syntax,
though, is the *frame* syntax; each frame is defined by a Clojure *keyword*.
There are four logical frames for classes, `:super`, `:sub`,`:equivalent` and
`:disjoint`. The simplest use of these is with another named class. For
example, using a Pizza example, cheese toppings could be defined as follows:

    (defclass CheeseTopping)

    (defclass GoatsCheeseTopping
        :super CheeseTopping)
    (defclass GorgonzolaTopping
        :super CheeseTopping)
    (defclass MozzarellaTopping
        :super CheeseTopping)
    (defclass ParmesanTopping
        :super CheeseTopping)

Likewise, equivalent and disjoint classes can be expressed in this way.
For named classes, it is often easier to use `as-subclasses` macros.

## Class Expressions

Frames are at the most useful when used with arbitrary classes
expressions which Tawny-OWL also supports.

For example, this example creates a *defined* class called `CheesyPizza`,
which is equivalent to a `Pizza` which `hasTopping` of `CheeseTopping`.

    (defclass CheesyPizza
        :equivalent
        Pizza
        (owl-some hasTopping CheeseTopping))

In this case, an existential restriction is created using the `owl-some`
function. This is semantically equivalent to this form:

    (defclass CheesyPizza
        :equivalent
        (owl-and Pizza
            (owl-some hasTopping CheeseTopping)))

which also makes use of an explicit `owl-and`. All of the class constructors in
OWL are supported, in many cases with a [variety](nameclashes.md) of names.
These are:

- `owl-some`
- `only`,`owl-only`
- `owl-not`, `!`
- `owl-or`, `||`
- `owl-and`, `&&`
- `at-least`
- `at-most`
- `exactly`
- `has-value`
- `has-self`

There is also a single convenience constructor: `some-only` which combines
both `some` and `only` property restrictions in a single step.

## Forward Declarations

Unlike OWL itself, Tawny-OWL requires symbols (i.e. classes or properties) to
have been created before they are used. A more naturalistic interpretation of
OWL would allow statements like so:

    (defclass A
        :super (owl-some hasPart B))
    (defclass B)
    (defoproperty hasPart)

Actually, something equivalent to this is possible in Tawny-OWL as we shall
see later. However, this exact code will return an error `Cannot find symbol
hasPart`. Symbols must be defined before use.

There are two solutions to this problem; the first, obviously, is to reorder
your statements, which would work well in this case.

    (defoproperty hasPart)

    (defclass B)
    (defclass A
        :super (owl-some hasPart B))

In many cases where classes need to refer to each other this is often not
possible. For instance:

    (defclass A :disjoint B)
    (defclass B :disjoint A)

The most common of these cases (disjoints as shown) can be avoided with the
`as-disjoint` macro:

    (as-disjoints
        (defclass A)
        (defclass B))

When all else fails, the problem can be over come by using some combination of
the `def-*` macros and their equivalent functions (described in more detail
in the next section).

    (defoproperty hasPart)
    (defclass B)
    (defclass A
        :super (owl-some hasPart B))
    (owl-class B :super
        (owl-some isPartOf A))

In this case, we define `B` with no frames, and then refine it later, adding
more frames to the same entity. Tawny-OWL also provides a convenience
`declare-classes` macro which allows the definition of multiple classes at
once.

    (declare-classes B C)
    (defclass A
        :super (owl-some hasPart B C))
    ;; more definitions of B and C
    (owl-class B :super
        (owl-some isPartOf A))
    (owl-class C :super
        (owl-some isPartOf A))

## Tawny without variables

So far the documentation has described the use of Tawny-OWL as a DSL. For each
new class or property, a new Clojure variable is created also. This works well
for ontology development, but it not so useful for use of Tawny-OWL as an API.
By definition this is a programmatic use of Tawny-OWL, so this
section assumes a reasonable understanding of Clojure and its related
terminology.

For each of the macros described so far, there are equivalent functions. These
simply return the OWL API objects created -- the macros forms such as `defclass`
return the Clojure [Var](http://clojure.org/vars) object for consistency with
the rest of Clojure. For example:

    (owl-class "A" :super "B" "C")

will return a class with name "A", a subclass of "B" and "C". If "B" and "C"
do not exist, then these will also be created. These functions are not pure,
they affect the current ontology even if the return values are not kept,
affecting all subsequent calls. The function is, effectively, idempotent
however so multiple calls will return the same class. It is possible to mix
this form of call freely with the `defclass` forms:

    (defclass A)
    (owl-class "B" :super A)
    (defclass C :super "B")

This is not a particularly good idea from an engineering point of view. It is
also important to note that the using strings to define classes and other
parts of OWL will mean that some probable errors will be missed. For example,
here we try to make a subclass `A` with a property `B`. Tawny-OWL will prevent
this.

    tawny.owl> (defoproperty B)
    #<OWLObjectPropertyImpl <http://iri/#B>>
    tawny.owl> (defclass A :super B)
    IllegalArgumentException Expecting a class. Got: <http://iri/#B>
        tawny.owl/ensure-class (owl.clj:356)

The equivalent string based system does not:

    tawny.owl> (object-property "B")
    #<OWLObjectPropertyImpl <http://iri/#B>>
    tawny.owl> (owl-class "A" :super "B")
    #<OWLClassImpl <http://iri/#A>>

Any attempt to reason over an ontology with these statements is likely to
complain, though, that B is both a class and a property. The first advantage of
this approach, though is that it's easier to code again. The macro forms
require symbols as their first parameter, which requires the use of macros and
back-tick unquoting to use programmatically. The second advantage is that the
macros require forward declaration which means building an ontology needs to
be done incrementally. One piece of the Tawny-OWL test code reads an OWL file,
renders it into Clojure, then reads it back again; it does this using the
function form to avoid the forward declaration problem.

Performance wise, both are about the same.

## Next

 - [Querying](querying.md)
 - [Reasoning](reasoning.md)
