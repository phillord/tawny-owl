Release Notes
=============

For future functionality please see [roadmap](roadmap.md)


# 1.4

The 1.4 release extends Tawny's existing pattern functionality. Patterns
created by tawny can now be annotated as belonging to a particular instance of
pattern, which in turn makes it easy to find all the entities created by a
pattern. As this capability uses OWLs annotation capabilities, this means that
the pattern instances are also apparent in the saved OWL ontology so other
tools can use this information also.

Tawny-OWL is not also partially documented using
http://github.com/phillord/lentic[lentic]. While this is not complete, it does
not make sense to delay the release until this is ready as it is developer
documentation.

## New Features

- Support for "facets" -- an easy mechanism for making existential
  relationships with a set of classes.
- The addition of `tawny.pattern/pattern-annotator` and associated functions,
  which links together all the entities in a pattern.

## Bug Fixes

- Tawny now guesses the type of a statement better by looking at all
  arguments.

# 1.3

The 1.3 release adds three new features. First, we add support for axiom
annotations. While this has always been possible through the direct use of the
OWL API, this also circumvented many of the features of Tawny-OWL. This is now
possible, through use of the `tawny.owl/annotate` function.

Second, support for building patterns has been reworked in the `tawny.pattern`
namespace. The functions `p` and `e` allow easy expression of patterns in a
way which can cope easily with optional frames, and also integrates well with
`pattern-generator` which allows easy construction of macro'd versions of
function based patterns; this allows easy interning of pattern entities and
the use of symbols.

Finally, a new `annotator` function has been added to `tawny.owl`. This allows
easy construction of short-cut functions like `label` and `owl-comment` which
construct annotations of a specific type, while still supporting Tawny-OWLs
default ontology functionality. The motivation for this functionality comes
from Jennifer Warrender, who has shown that it is a common pattern in several
ontologies.



## New Features

- It is now possible to add annotations to axioms as well as entities. The
  primary entry point is `annotate`.
- A number of new functions have been added to `tawny.pattern`.


## Breaking Changes

- `tawny.pattern` has been re-written, meaning that `value-partition` is now a
  function, with `defpartition` offering the same thing in macro form.


# 1.2

The main feature for the 1.2 release has been the incorporation of core.logic,
through (ab)use of the Tawny's querying facilities. A tighter integration
should be possible, having core.logic work directly over the OWL API, but this
was relatively simple to implement. It is performant enough for most uses (the
Gene Ontology renders to Clojure data structures in 1-2 seconds on my
desktop).

Currently, this integration is used only by `tawny.query`, providing a query
language for searching over the OWL data model. It should also pave the way
for repurposing of other tools, though, including a linter like `kibit`.

One other substantial change is an aggressive micro-optimisation of
default-ontology and broadcast-ontology functionality. These functions are
called for almost every user function in `tawny.owl` but had a base call time
in the microsecond range (rather than 10s of nanoseconds for a normal call).
These micro-optimisations cost a slightly longer code-base; however they give
a 3-5x speed increase. Loading a tawny rendered version of GO is now about
only about 3x slower than reading the OWL file. These optimisations have
resulted in two breaking changes to the broadcasting and default ontology
behaviour, both of which now have alternatives.

Finally, rendering has now been cleaned up, so that the ontology form can also
be rendered, and this form re-read. This has required a few additional changes.

## New Features
- `tawny.query` now includes a number of `core.logic` predicates. 
- `tawny.render/as-form` can now cope with any OWL object.
- `tawny.render/as-form` now takes options, producing output in several
  formats: two for evaluation or documentation, and two for querying. It is
  also possible to select the set of ontologies or ontology manager with which
  to render.
- `tawny.render` can now render OWLOntology objects.
- The `ontology` function now supports :import and :annotation frames which
  broadcast.
- `direct-instances` function added.
- `defentity` form added and made public.
- We now test against several versions of the OWL API.


## Bug Fixes
- Tawny's own annotation ontology now uses `defonce` semantics which makes
  reloading easier.
- :versioninfo handling was broken on ontology.
- `refine` now uses default ontology
- `ontology` now adds a prefix even if one is not specified. This avoids a NPE
  when saving in a prefix format.
- `as-form` now renders multiple facts correctly
- Several bugs fixed with property chain support, as well as their rendering.
- Numeric restrictions now accept doubles
- `tawny.fixture/ontology-and-reasoner` didn't actualy work

## Breaking Changes

- broadcasting functions no longer nil patch; previously nils were silent
  ignored. In practice, this was added for internal reasons (which are no
  longer necessary), and was probably a bug rather than a feature. Nils are
  now passed to OWL API, and generally cause fail early NullPointerExceptions.
- The default-ontology and broadcast-ontology mechanisms have been extensively
  micro-optimised; to enable this optimisation, I have deprecated one
  "feature" which was largely undocumented. Many functions which previously
  accepted an :ontology frame, no longer do, but still accept an ontology as
  the first argument. The `defentity` forms still accept this (as they must
  have a symbol as the first argument) but the :ontology frame *must* come
  first and have a single ontology.
- `tawny.query` has been extensively reworked, including changes to the
  underlying representation.
- `tawny.render/form` has gone (private). Extensions to `tawny.render/as-form`
  should work as a replacement.
- `tawny.render/as-form` is no longer a multimethod, just a function. In
  practice, this should have been an implementation detail anyway.
- Various dynamic vars in `tawny.render` have gone, replaced by options in the
  `as-form` call.
- `tawny.render/as-form` now returns a lazy list. In practice, this is should
  be breaking change only if you use `str` (in which case change to `pr-str`
  instead). The laziness may also cause some unpredictable issues if you store
  the form, and change the ontology, then realise the form.
- A number of functions which used to work on `OWLNamedObject` now support
  `OWLOntology` also. These include `tawny.lookup/iri-to-var` and associated
  functions.

## Dependencies

- OWL API to 3.4.10
- Hermit to 1.3.8.4
- New dependency: core.logic 0.8.7

# 1.1

The 1.1 release has largely been about regularisation of the syntax, enhanced
performance and added a few pieces of OWL missing from 1.0.

Regularisation is the biggest change and is, unfortunately, a breaking change.
Original Tawny mimicked Manchester syntax; it's "subclassof" frame is
back-to-front -- the filler is a superclass of the entity in question. Tawny
now uses ":sub", ":super" as slot names; ":subclass" and ":subproperty" are
deprecated and will be removed at a later date.


## Documentation
 - Have added more exemplar ontologies. The OWL primer ontology has now been
   ported to Tawny.

## New Features
 - Individual facts can now be defined by the `is` function (rather than
   `fact`) and `owl-not` (or `tawny.english\not`).
 - Introduced new functions for declaring disjoint object and data properties.
   `as-disjoint` now supports all of these.
 - Introduced new function, `as-equivalent` which works for classes, object or
   date properties.
 - `suboproperties', `suboproperty?' and related functions added.
 - Both `equivalent?` and `disjoints?` now work for data and object properties
   also.
 - `inverse` function allows for anonymous reference to the inverse of a
   property expression.
 - `oneof` now takes accepts strings, numbers and booleans as literals without
   requiring `literal`.
 - Unqualified cardinality is now supported without using of `owl-thing`
 - The JFact reasoner is now supported.
 - New aliases have been added to `tawny.english`.
 - All namespaces now load without reflection warnings, having been heavily
   type-hinted. This should introduce no changes but many namespaces run much
   faster; `tawny.owl` about 2x, `tawny.render` perhaps 5x for very
   approximate benchmarks.

## Breaking Changes
 - `owl-not` now also supports individuals -- this is technically a breaking
   change because `owl-not` previously had arity 1, now also has arity 2.
 - `disjoint-classes` list has been removed, and `disjoint-classes-list` has
   been renamed to `disjoint-classes`!
 - `tawny.protege-nrepl` has now been removed; its had dependencies that were
   not explicitly specified as it ran inside protege. It is now in a project
   of its own, called `tawny.protege`.
 - `add-subclass` functionality has now reversed its functionality -- the
   second class is now the subclass rather than the superclass.

## Bug Fixes
- rendering of object and data properties now reports superproperties.
- `superclasses` and `subclasses` no longer crash on circular class hierarchies
- `datatype` equivalents were limited to other datatypes. Now any datarange
   is usable.

## Dependencies
 - OWL API to 3.4.9. Hermit to 1.3.8.3
 - Clojure 1.6

# 1.0

## Code Changes
 - Introduced `integration_test` namespace for errors that could be in several
   places.

## Bug Fixes
 - `iequivalent-classes` was removing top and bottom. This makes sense
   `isuper` and `isubclasses` but was breaking a pitfall example.
 - `individual-explicit` was incorrectly declared as variadic and so silently
   ignoring all frames.
 - `with-suffix` and `with-prefix` were only returning their last form which
   prevented their use within `as-disjoints` and equivalent.
 - `tawny.owl` was not necessarily being loaded correctly, leading to profile
   violations. It is not loaded as a class resource lazily when necessary.
 - `tawny-emacs.el` was crashing with "Track Entites" should now be fixed.

# 1.0 (rc1)

## Code Changes
 - Lookup implementation has been enhanced for performance. This also affects
   repl and render namespaces.
 - All relevant functions should broadcast now.

## Bug Fixes
 - Support for datatypes was broken in a few areas.

## Breaking Changes

 - `remove-axiom` now takes a list, which most of the `add-*` functions returns
 - characteristics on properties now use keywords (:functional instead of functional).
 - function names have been regularised (see
   [documentation](nameconventions.md)) resulting in many name changes.
 - The format for `memorise.clj` has changed significantly; this allows
   arbitrarily sized files to be created. There is no converter from the old
   file format, but I will write one if it is needed.
 - The format for `obo.clj` has changed to using EDN, rather than a bespoke
   properties file. As with `memorise.clj` there is no converter.

# 1.0

## New Features

 - tawny-emacs.el now has explicit support for Protege
 - A new `protege-nrepl.clj` namespace which hooks tawny into core Protege
   data structures.

# 0.12

This release is intended to be feature complete. New features will not be
added for 1.0, although accidentally missing functionality may be added.

## New Features

- Complete support for OWL 2, include data types
- OWL documentation can be queries as normal clojure metadata
- New namespaces, query and fixture
- Completion of rendering functionality
- Regularisation of interfaces: where relevant functions now take an ontology
  as  the first argument.
- Updated to Hermit 1.3.7.3, OWL API 3.4.5

## Breaking Changes

This version introduces a few breaking changes.

- The `as-subclasses` and `as-disjoint` macros are now functions, making them
  lexically scoped.

# 0.11

## New features

- facts on individual are now supported
- documentation has been greatly extended
- OWL API 3.4.4

## Breaking Changes

This version introduces a few breaking changes.

- `isubclasses`,`isuperclasses` have been changed to `direct-subclasses`,
  `direct-superclasses` in `tawny.owl`. This was to avoid a nameclash with
  `tawny.reasoner`
- `*reasoner-progress-monitor*` is now an atom. While this enables the use of
  `reset!`, it means an atom must be used in `binding`. Samples are given in
  the documentation
