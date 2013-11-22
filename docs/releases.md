Release Notes
=============

# 1.0.1

## Bug Fixes
 - `datatype` equivalents were limited to other datatypes. Now any datarange
   is usable.
=======
## New Features
 - Introduced new functions for declaring disjoint object and data properties.
   `as-disjoint` now supports all of these.
 - Introduced new function, `as-equivalent` which works for classes, object or
   date properties.
 - Both `equivalent?` and `disjoints?` now work for data and object properties
   also.
 - `inverse` function allows for anonymous reference to the inverse of a
   property expression.
 - `oneof` now takes accepts strings, numbers and booleans as literals without
   requiring `literal`.
 - Unqualified cardinality is now supported without using of `owl-thing`

## Breaking Changes
 - `disjoint-classes` list has been removed, and `disjoint-classes-list` has
   been renamed to `disjoint-classes`!


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
