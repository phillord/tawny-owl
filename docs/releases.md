Release Notes
=============


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
