Polyglot
========


Polyglot is a simple solution to the issues of multi-lingual ontologies.
It was mostly developed as a mechanism for exploring the programmatic nature
of tawny, rather than a serious attempt to add heavy-duty multi-lingual
support; never the less, we describe it here as it may be useful for simple
cases.

The namespace consists of two entry points, `polyglot-load-label` and
`polyglot-create-resource`. We describe it use with the tawny pizza ontology.
First, after creating an existing resource, provide a form to generate an
outline resources file.

    (tawny.polyglot/polyglot-create-resource
        "src/pizza/pizzalabel_it.properties")

This is created in the `src` directory so that it will be versioned along with
the rest of the source code. The file name can be anything you wish, although
we have used a standard form here. The file created will look something like
this:

    AnchoviesTopping=
    ArtichokeTopping=
    AsparagusTopping=

The Clojure identifiers are used as keys; note this means `polyglot` will only
work with entites present in the namespace. This function will overwrite any
existing file, so in practice it is likely only to called from the
[repl](repl.md).

Labels are loaded using a similar command, put including an language code,
which is used directly in the OWL API.

    (tawny.polyglot/polyglot-load-label
      "pizza/pizzalabel_it.properties" "it")

This command issues warnings about any missing labels. Although it was
designed to operate over ontologies defined using the tawny language, in could
be used over an ontology [read](read.md) from an OWL file also.
