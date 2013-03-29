Adding Restrictions
===================


## Frames

Tawny-OWL has a fairly flexible syntax, and provides a number of mechanisms
for specifying restrictions to classes and properties, including the macros
covered in [getting started](getting-started.md). The most basic syntax,
though, is the *frame* syntax; each frame is defined by a Clojure *keyword*.
There are three logical frames for classes, `:subclass`, `:equivalent` and
`:disjoint`. The simplest use of these is with another named class. For
example, using a Pizza example, cheese toppings could be defined as follows:

    (defclass CheeseTopping)

    (defclass GoatsCheeseTopping
        :subclass CheeseTopping)
    (defclass GorgonzolaTopping
        :subclass CheeseTopping)
    (defclass MozzarellaTopping
        :subclass CheeseTopping)
    (defclass ParmesanTopping
        :subclass CheeseTopping)

Likewise, equivalent and disjoint classes can be expressed in this way.
For named classes, it is often easier to use `as-subclasses` macros. However,
frames are at the most useful when used with arbitrary classes expressions
which Tawny-OWL also supports.

For example, this example creates a *defined* class called `CheesyPizza`,
which is equivalent to a `Pizza` which `hasTopping` of `CheeseTopping`.

    (defclass CheesyPizza
        :equivalent
        Pizza
        (owlsome hasTopping CheeseTopping)))

In this case, an existential restriction is created using the `owlsome`
function. This is semantically equivalent to this form:

    (defclass CheesyPizza
        :equivalent
        (owland Pizza
            (owlsome hasTopping CheeseTopping)))

which also makes use of an explicit `owland`. All of the class constructors in
OWL are supported, in many cases with a [variety](nameclashes.md) of names. These are:

- `owlsome`
- `only`,`owlonly`
- `owlnot`, `!`
- `owlor`, `||`
- `owland`, `&&`
- `atleast`
- `atmost`,
- `exactly`

*Note* `hasValue` and `hasSelf` haven't been written yet. 

There is also a single convienience constructor: `someonly` which combines
both `some` and `only` property restricutions in a single step. 
