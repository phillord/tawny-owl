Nameclashes
===========

One of the idiosyncrasies of Tawny-OWL is that several user facing functions
have multiple names, and that these are somewhat inconsistent: compare, for
instance, `owlsome` and `only`.

This document explain the design rationale behind this.

## The problem

While using a full programming language and a DSL has some significant
advantages over, for example, Manchester syntax. However, there are also
disadvantages as well; a key one is that the DSL shares it symbols with the
general purpose section of the language. This raises the possibilities of
nameclashes.

Rather like Java's `java.lang`, Clojure contains a namespace that is
automatically imported, that is `clojure.core`. This namespace is relatively
large, containing many hundreds of functions and some of these use names that
by default, I would have used for Tawny-OWL. These are: `some`, `or`, `and`
and `not`. Using this names in Tawny-OWL would require some special handling.
The options are described here.

## Namespacing

Clojure contains a namespacing mechanism which can be used to circumvent this
problem. This mechanism allows for example, the reuse of the name `replace`
between `clojure.core` and `clojure.string`. To avoid namespace warnings when
ever `tawny.owl` is required `refer-clojure` would be needed:

    (ns tawny.pizza
        (:refer-clojure :exclude [some or not and])
        (:use [tawny.owl]))

This is effective, but the `refer-clojure` form must be used everytime
`tawny.owl` is used, or a somewhat cryptic warning results. It also means that
every use of `and` and `or` as logical operators within Clojure must be
namespace qualified.

In practice, using `refer-clojure` is sometimes necessary anyway; for
instance, with `tawny.obo.obi` this is required because some of the OBI
classes clash with clojure names: specifically `binding`. This is actually
less common than might be for reasons of naming convention, as Clojure uses a
lower-case-with-hyphen convention, while ontologies use either a
words_with_underscore or CamelCase convention. It is to avoid this necessity
that we have avoided using clashing OWL functions.

## Using Types

In a statically typed language, the problem of nameclashes is not as
significant, since the nameclashing method can be tied to a specific class.
So, `and` would operate as a boolean operator over booleans, and a OWL boolean
class constructor over OWL Classes. This is relatively easy to do in Scala,
for instance, although as this uses C style logical operators, the problem is
less anyway.

Although, Clojure is not statically-typed, all the objects created by
Tawny-OWL are Java, and do have runtime types. So, it would be possible to
build functions that operate like `owland` when used with OWL objects, and
like `and` for anything else. This would avoid the problem of requiring
namespace qualification for use of `and` and `or`.

However, this approach seemed a little but too hacky; having one function do
two very different things seems a bit dubious, particularly as a logical and
of two variables potentially containing OWL Classes is something that a
programmer might want to do. It also still requires use of `refer-clojure`
which makes things worse.

## Using a prefix

The third option is use of a prefix. The OWL API takes this approach, tacking
"OWL" onto the start of almost everything. While this disambiguates, it is not
a very clean approach, adds to the length of everything and generally causes
confusion.

## The Tawny-OWL approach

Tawny-OWL takes the approach of not using namespaces to differentiate, but
using a prefix where necessary, and providing the option of using it always.
So, for instance, there is a core function called `some`, hence the Tawny
function is `owlsome`. There is not core function called `only`, hence Tawny
provides two names (to the same function), `owlonly` and `only`. The logical
constructors all provide their C style equivalent: `owland` and `&&`.

We are still not entirely convinced that this is the correct approach; a
boilerplate `:refer-clojure` form can be provided, and as noted the `:only`
form is sometimes necessary anyway. We may switch to providing the bare forms
also. Regardless, the `owl` prefixed forms will remain.
