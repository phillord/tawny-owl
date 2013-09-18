Release Notes
=============

# 1.0

## Code Changes
 - Lookup implementation has been enhanced for performance. This also affects
   repl and render namespaces.

## Bug Fixes
 - Support for datatypes was broken in a few areas.

## Breaking Changes

 - characteristics on properties now use keywords (:functional instead of functional).
 - function names have been regularised (see
   [documentation](nameconventions.md)) resulting in many name changes.

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
