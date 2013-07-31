Scripting with Tawny
====================

One of the design choices behind tawny, was to build a flexible and
comfortable syntax for writing OWL ontologies, using a full programming
language, as opposed to writing an OWL syntax and supplementing it with
programming features. This makes the tawny syntax both fully extensible and
arbitrarily scriptable. In this document, we consider these advantages.

Scripting is not that complex but it does require a reasonable knowledge of
either lisp or Clojure to use beyond a trivial level. We assume that knowledge
here.

## With or without vars

As described [previously](adding-restrictions.md), tawny can be used purely as
an API -- adding and [removing](repl.md) entities from the ontology. In
addition to this, it provides a set of macros which add vars to the local
namespace. For scripting, it is important to understand the distinction
between these two. In general, it is a little easier to script without
creating vars, and if it is not necessary to have these vars, it is the best
way to do so. However, this will have implications for ontology builders using
the classes created in this way; in particular, they will not be able to
refer to them as symbols, and tools such as auto-complete, or native
[documentation lookup](repl.md) will not work. We have, however, added
relatively easy features to using vars, so that you are not forced into using
macros and manipulating symbols which is a painful way to do things.

## Building multiple classes at once.

The most common requirement is to simply create multiple new concepts at once.
This is relatively simply to do in Clojure is to use a list comprehension.
Here is a simple example from the pizza ontology. This also adds a standard
prefix for us, which saves a bit of typing.

    (doseq
        [n ["Carrot"
            "CherryTomatoes"
            "KalamataOlives"
            "Lettuce"
            "Peas"]]
      (owlclass (str n "Topping") :subclass VegetableTopping))

This version does *not* create vars, so attempts afterwards to use
`CarrotTopping` will fail. It is relatively easy to add vars also, using a
`intern-entity` from `tawny.read`.

    (doseq
       [n
         ["ChilliOil"
          "Chives"
          "Chutney"
          "Coriander"
          "Cumin"
          ]]
      (tawny.read/intern-entity
       (owlclass (str n "Topping") :subclass VegetableTopping)))

This achieves the same thing, but also adds vars. The same thing can be done
with properties, by using the `objectproperty` function. So far, we have never
needed to do this, as most ontologies are class heavy, rather than property.

Clojure programmers might ask, why not just use `clojure.core/intern`; to
which the answer is, it's nearly the same, but not quite because it adds some
metadata to the symbol, and runs some checks. Check the code if you want!

## Laziness

One of the really interesting things about Clojure is it's support for
laziness. If you are not interested, please skip to the last paragraph of this
section, which gives some easy rules to follow. If you are interested in why
the rules are there, read on.

It is possible to express infinitely long sequences for instance; but,
unfortunately, this breaks everything when using Java objects which is how
tawny works. So, the obvious approach of using `map` fails. For instance,
adding this to your file

    (map
        #(owlclass %)
        ["a" "b" "c" "d"])

will achieve nothing. This is because `map` is a lazy list. Alternative
consider this (slightly elided) [REPL](repl.md) session.

    (test-ontology)
    ;; #<OWLOntologyImpl [Axioms: 0 Logical Axioms: 0]>
    (def x (map #(owlclass %) ["a" "b" "c"]))
    ;; #'user/x
    (get-current-ontology)
    ;; #<OWLOntologyImpl [Axioms: 0 Logical Axioms: 0]>
    (map #(owlclass %) ["a" "b" "c"])
    ;; (#<OWLClassImpl a> #<OWLClassImpl b> #<OWLClassImpl c>)
    (get-current-ontology)
    ;; #<OWLOntologyImpl [Axioms: 3 Logical Axioms: 0]>

Our first attempt to use `map` produces no effects at all. Again, the value
placed in the variable `x` is lazy, and until the list is evaluated nothing
happens. The value returned by the second `map` is automatically evaluated,
but *only* because we are using the REPL. You can see the same thing in this
code; after evaluating `x` at the REPL, it all works.

    (test-ontology)
    ;; #<OWLOntologyImpl [Axioms: 0 Logical Axioms: 0]>
    (def x (map #(owlclass %) ["a" "b" "c"]))
    ;; #'user/x
    (get-current-ontology)
    ;; #<OWLOntologyImpl [Axioms: 0 Logical Axioms: 0]>
    x
    ;; (#<OWLClassImpl a> #<OWLClassImpl b> #<OWLClassImpl c>)
    (get-current-ontology)
    ;; #<OWLOntologyImpl  [Axioms: 3 Logical Axioms: 0]>

The practical upshot of all this is, while `doseq` has a slightly more complex
syntax than `map`, it's better to use this. If you use `map` or any of
Clojure's lazy constructs, you need to use `doall` regularly.

## Extending

It is possible to arbitrarily extend `tawny.owl`, to add new features and
functionality, to suite specific workflows. For example, we have done this
with `obo.clj` to add specialise support for identifiers. This form of
extension is relative complex, requiring knowledge of both clojure, and the
OWL API.

However, not all extension are complex. The most obvious example is to
essentially add new syntax. For consider this annotation property from
`tawny.upper`.

    (defannotationproperty Scope
      :subclass owlcommentproperty
      :comment "The scope provides context to the definition and describes that
      parts of the domain which are (or are not) intended to be described by definition.")

    (def scope
      (partial tawny.owl/annotation Scope))

By adding function `scope`, we can easy the use of the `Scope` property,
reducing the necessity for the direct use of the property.

    (defclass A
        :annotation (annotation Scope "An long-off test class"))
    (defclass B
        :annotation (scope "A one-off test class"))

This kind of extension is minor, but the code is easy to write and,
critically, can be included in the same file as the ontology. In this way, an
tawny file can become an amalgam of ontology and functions, seamlessly
integrated.

This form of extensibility is also used in `tawny.pizza` where a new function
is created to describe a pizza.

    (defn generate-named-pizza [& pizzalist]
      (doall
       (map
        (fn [[namedpizza & toppings]]
          (add-subclass
           namedpizza
           ;; use apply because we have a single list, someonly expects a list of
           ;; arguments.
           (apply someonly
                  ;; toppings is alread a list!
                  (cons hasTopping toppings))))
        pizzalist)))

    (generate-named-pizza
        [CapricciosaPizza AnchoviesTopping MozzarellaTopping
         TomatoTopping PeperonataTopping HamTopping CaperTopping
         OliveTopping]

which expands to this definition in Manchester syntax.

    Class: piz:CapricciosaPizza

        SubClassOf:
            piz:hasTopping some piz:CaperTopping,
            piz:hasTopping some piz:MozzarellaTopping,
            piz:hasTopping only
                (piz:AnchoviesTopping
                 or piz:CaperTopping
                 or piz:HamTopping
                 or piz:MozzarellaTopping
                 or piz:OliveTopping
                 or piz:PeperonataTopping
                 or piz:TomatoTopping),
            piz:hasTopping some piz:OliveTopping,
            piz:hasTopping some piz:HamTopping,
            piz:NamedPizza,
            piz:hasTopping some piz:PeperonataTopping,
            piz:hasTopping some piz:AnchoviesTopping,
            piz:hasTopping some piz:TomatoTopping


In this case, this saves quite a considerable amount of typing, and also
reducing the risk of incidentally errors, as well as leaving a clearer and
declarative statement of the intention for the use of the `NamedPizza`
concept.

## Incrementally Defining Entities

It is possible to build entities up incrementally; this is often very useful
where forward references are needed. Consider this somewhat elided example
from the `tawny.pizza`

    (defclass Pizza)
    (defoproperty hasTopping
      :domain Pizza)
    (defoproperty hasBase
      :domain Pizza)
    (owlclass Pizza
              :subclass
              (owlsome hasTopping PizzaTopping)
              (owlsome hasBase PizzaBase))

In this case, we define the `Pizza` class and place the OWLObject into the
`Pizza` var. We use this value to define two properties `hasTopping` and
`hasBase`. Then we extend the definition of `Pizza` to use these two
references; in this case, we use `owlclass` rather than `defclass` as we have
already created a class in the `Pizza` var; in practice, we could also use
`defclass`, which would redefine the var. Either should operate in the same
way.


## Refining

Tawny also provides a `refine` function which can be used to update an owl
object regardless of its type. This can be useful, for example, for adding
annotation. Consider this example from the `pizzaontology`.

    (doseq
        [e (.getSignature pizzaontology)
         :while
         (and (named-object? e)
              (.startsWith
               (.toString (.getIRI e))
               "http://www.ncl.ac.uk/pizza"))]
      (try
        (println "refining:" e)
        (refine e :annotation (annotation creator "Phillip Lord"))
        (catch Exception exp
          (printf "Problem with entity: %s :%s\n" e exp)
          )))

The first half just filter the relevant entities from the ontology, while the
`refine` form adds an annotation; without `refine`, a type check would be
necessary and the relevant function called.


