Release Notes
=============

# 1.1

The 1.1 release has largely been about regularisation of the syntax, enchanced
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
   arbitarily sized files to be created. There is no converter from the old
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
