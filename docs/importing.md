Importing and Reading
=====================

Using other ontologies is an important part of the development process. Tawny
provides three key ways to achieve this, and they are distinctly different.
*Importing* is a key part of the OWL language; *reading* is a part of how
we have integrated OWL with tawny; and *loading* is how the underlying OWL API
interacts with an OWL file.


## Loading

Loading an ontology describes the process of changing an OWL file on a disk
into a `OWLObject`; these objects are defined by the underlying OWL API and
are used directly by tawny also.

Tawny provides no specific support for loading an ontology, as it is
straight-forward to do using the OWL API directly. Consider this example, from
`tawny.read-test`.

    (defn get-go-ontology []
      (tawny.owl/remove-ontology-maybe
       (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))
      (.loadOntologyFromOntologyDocument
       tawny.owl/owl-ontology-manager
       (IRI/create (clojure.java.io/resource "go-snippet.owl"))))

This loads a small section of the Gene Ontology for use in various test cases.
Broken down:

    ;; Create an ID which is the same as that given in the go-snippet.owl file
    (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")

    ;; if it exists, remove this ontology from the ontology manager. There
    ;; can only be one at once.
    (tawny.owl/remove-ontology-maybe
       (OWLOntologyID. (IRI/create "http://purl.obolibrary.org/obo/go.owl")))

    ;; create a physical IRI describing the location of the file
    ;; go-snippet.owl
    (IRI/create (clojure.java.io/resource "go-snippet.owl"))))

    ;; load this into memory as an OWLOntology
    (.loadOntologyFromOntologyDocument
      tawny.owl/owl-ontology-manager
      (IRI/create (clojure.java.io/resource "go-snippet.owl")))

This object can be used in every way like an Ontology object created directly
by tawny. For highly programmatic uses of tawny, this may be the best way to
load a file.

## Importing

The process of "importing" an ontology is entirely different. First, an
`OWLObject` must already exist, created by one of the mechanisms described in
this document. Importing in this sense means making axioms in the imported
ontology available in the current ontology. It can be achieved with a single
OWL form.

    (owlimport pizza/pizza)

The process of importing an ontology is critical if you wish it to impact on
reasoning. If we wish to use classes, for instance, in the pizza ontology, we
must both `require` or `use` these classes, and `owlimport` them. For example:

    (ns pizza.mypizza
      (:use [tawny.owl])
      (:require [pizza.pizza :as p]
                 [tawny.reasoner]))

    (defontology mypizza
      :iri "http://mypizza"
      :prefix "my:"
      )

    ;;(owlimport p/pizzaontology)

    (defclass ImpossiblePizza
      :subclass p/VegetarianPizza
      (owlsome p/hasTopping p/HamTopping))


    (tawny.reasoner/reasoner-factory :hermit)
    (println (tawny.reasoner/coherent?))

It might be expected that these statements would print `false` -- that the
ontology is not coherent. In fact, with the `owlimport` statement commented
out, `ImpossiblePizza` is satisfiable -- while the concepts share the same
IRIs as those from the pizza ontology this is *all* that they share; the
axioms describing `VegetarianPizza` and `HamTopping` are not included. With
the `owlimport` statement the ontology behaves as might be expected, and is
incoherent.


## Reading

Tawny normally works by creating Clojure vars for each entity created in an
ontology. Computationally, these entities are `OWLObject` instances from the
OWL API. As described
[earlier](adding-restrictions.md#tawny-without-variables), it is possible to
use tawny without these variables, and instead using strings. So, for example,
we could do the following:

    ;; using the function from above
    (owlimport (get-go-ontology))

    (defclass A
        :subclass
            (owlclass ""
                :name "http://purl.obolibrary.org/obo/GO_0000002"))

This would create a new class called "A" which is a subclass of `mitochondrial
genome maintenance` as defined in GO. This however has two problems with
should be apparent from the code snippet. First, the OWL class must be
referred to with a string; spelling mistakes will result in a new class,
rather than reusing the existing one. And, second, the URI has been
used directly. So, interacting with an ontology loaded in this way happens in
a very different way to those defined with tawny. However, we provide a second
function to read an OWL file so that it appears as a full tawny library. We
provide a full example here, importing the OBI ontology. This is based on an
[article](http://www.russet.org.uk/blog/2316) published earlier on in my work
journal; the code here is somewhat updated due to changes in tawny.

    (tawny.read/defread obi
      ;; something that the OWL API can interpret. This includes a stream, so
      ;; it's totally generic.
      :location (IRI/create (clojure.java.io/resource "obi.owl"))
      ;; the prefix that you want to use in this case
      :prefix "obo"
      ;; normally only things from this IRI will be imported
      :iri "http://purl.obolibrary.org/obo/obi.owl"
      :viri "http://purl.obolibrary.org/obo/obi/2012-07-01/obi.owl"
      ;; but OBO ontologies are wierd, so pass in a filter function
      :filter
      (fn [e]
        (and (instance? OWLNamedObject e)
             (.startsWith
              (.toString (.getIRI e))
              "http://purl.obolibrary.org/obo/OBI"
              ))
        )
      :transform
      ;; fix the space problem
      (fn [e]
        (clojure.string/replace
         ;; with luck these will always be literals, so we can do this
         ;; although not true in general
         (.getLiteral
          ;; get the value of the annotation
          (.getValue
           (first
            ;; filter for annotations which are labels
            ;; is lazy, so doesn't eval all
            (filter
             #(.isLabel (.getProperty %))
             ;; get the annotations
             (.getAnnotations e
                              (tawny.owl/get-current-ontology))))))
         #"[ /]" "_"
         ))
      )

This is a fairly complex example, but it can be broken into pieces. The first
part of the form looks very similar to a `defontology` function call. The only
difference is that the `:iri` and `:viri` must match those in the file.
Currently, these must be duplicated from the file; this is because `defread`
unloads any ontology with the same IRI before it attempts to read this new one.

    (tawny.read/defread obi
      ;; the prefix that you want to use in this case
      :prefix "obo"
      ;; normally only things from this IRI will be imported
      :iri "http://purl.obolibrary.org/obo/obi.owl"
      :viri "http://purl.obolibrary.org/obo/obi/2012-07-01/obi.owl")

Unlike an `defontology` call which generates a totally empty ontology, we also
need to supply a location. This can be anything, including a stream, that the
OWL API can interpret. Here we provide two examples, one as a resource file
(which provides stability), and one a URL (which provides the latest version).

      :location (IRI/create (clojure.java.io/resource "obi.owl"))
      :location (IRI/create "http://purl.obolibrary.org/obo/obi.owl")

Unfortunately, the `obi.owl` file contains many classes from other ontologies,
such as Dublin Core, DOAP and IAO. In practice, we do not wish to read all of
these. By default, `tawny.read` only imports classes which start with the
ontology IRI -- a good heuristic but one which fails with OBI. Instead, we
apply a custom filter. This is applied to every entity, so here we check that
it starts with an OBI ID:

      :filter
      (fn [e]
        (and (instance? OWLNamedObject e)
             (.startsWith
              (.toString (.getIRI e))
              "http://purl.obolibrary.org/obo/OBI"
              ))
        )

Finally, we need to address a problem: OBI IRIs are numeric, and unreadable.
So, instead we need to apply a transform to produce a usuable string. In this
case, we take the label and remove any spaces.

      :transform
      ;; fix the space problem
      (fn [e]
        (clojure.string/replace
         ;; with luck these will always be literals, so we can do this
         ;; although not true in general
         (.getLiteral
          ;; get the value of the annotation
          (.getValue
           (first
            ;; filter for annotations which are labels
            ;; is lazy, so doesn't eval all
            (filter
             #(.isLabel (.getProperty %))
             ;; get the annotations
             (.getAnnotations e
                              (tawny.owl/get-current-ontology))))))
         #"[ /]" "_"
         )


Finally, we need to remember the names that are used and how they map to IRIs
in case the names change. For this, we use the `remember` function. For full
details of why this is necessary, please read my earlier
[article](http://www.russet.org.uk/blog/2316).

    (tawny.memorise/remember "./src/tawny/obi/obi_memorise.clj")

The filtering and transformations happen purely in `clojure` space. If an
ontology loaded in this way is saved using `tawny.owl/save-ontology` all
axioms will be saved, not just those filtered out.

## Next
