Using Numeric Identifiers
=========================

Numeric identifiers are often used for ontology terms. Here we describe how to
generate this form of identifier with Tawny.

## The problem

Numeric IDs can be very useful; because they are semantics free, it is
possible to change the identifying label without changing the underlying
semantics; this can be very useful for dependencies.

With Tawny, however this is painful. Numeric IDs render a programmatic
environment more or less impossible to use, with the added complexity that the
identifiers might not be legal Clojure identifiers.

## The Solution

Tawny provides a simple solution. Within the code base, human readable
identifiers are used to describe terms; these are then mapped to numeric IDs
automatically. The map is itself a text file which can be stored and versioned
alongside the rest of the code (while it is generated, it is source because it
is not necessarily regeneratable!).

Tawny uses a two-step process for coining new identifiers. By default, it uses
UUID based identifiers, which are generated randomly on the fly. At a later
date, probably when considering a release, these can be updated to permanent
numeric identifiers.

The reason that this workflow has been chosen is so that individual ontology
developers can work independently of each other, with only the coining of
final IDs requiring co-ordination. I have chosen this approach, as opposed to
using tools like URIgen, as the co-ordination and collaboration can happen
through a version control system; for a full explanation see my
[http://www.russet.org.uk/blog/2929](blog).

## In practice

A number of things need to happen for this to work. First, the iri-gen option
must be set when declaring the ontology. All the rest follows from this. Here
is an elided example from the
[https://github.com/phillord/tawny-obo-pizza](OBO pizza ontology)

    (defontology pizzaontology
      :iri "http://www.ncl.ac.uk/pizza-obo"
      :iri-gen tawny.obo/obo-iri-generate
      )


Next, we need to read in old iris which were set on a previous invocation. For
this, we call the `obo-restore-iri` function, with a file location.

    (tawny.obo/obo-restore-iri "./src/tawny/obo/pizza/pizza_iri.edn")

For this to work, I use a location next to the existing source, as this makes
it easy to version. Note that there is a bootstrap problem here; this file has
to exist to be restored. So an empty file needs to be created manually.

Next comes our ontology proper. Finally, we call the store-iri, to save any
IRIs that have been created (and the ones restored originally). While new IRIs
are random UUIDs they are persistant for a given identifier.

    (tawny.obo/obo-store-iri "./src/tawny/obo/pizza/pizza_iri.edn")

Finally, we can add a reporting method

    (tawny.obo/obo-report-obsolete)

This prints out messages about IRIs that exist, but where there is now no
equivalent entity.


## Caveats

There are a number of caveats to this process; how much of an issue this will
be is unclear, until the system has had more use.

1) The data is stored as a clojure data structure. I am not sure yet whether
multiple developers would produce lots of conflicts in VC. There are other
workflows possible -- UUIDs could be saved to a per developer file, with
restoration happening from all of them. It should be easy to investigate this
when the problem arises.

2) The current scheme for storing and loading can cause issues with
absolute/relative paths. Additionally, if the ontology is deployed as a jar
file (normal behaviour for a clojure dependency) attempt to store the data
every time is both unnecessary and will break. We will probably move to
discovering the file as a class resource, and saving conditionally.
