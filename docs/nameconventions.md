Naming Conventions
==================


The following naming conventions have been used in Tawny-OWL.

 - In general, `-` seperators have been used to split words in function names.
 - Where necessary `owl` prefixes have been used to distinguish from
   `clojure.core` functions. See [further documentation](nameclashes.md) for a
   rationale.
 - The exception to this is in the `def` macro forms. For some reason, in lisp
   these are generally not hyphenated.
 - The `def*property` macros use a single letter to distinguish between then;
   a compromise between typing and clarity.
 - `superclass`, `subclass` and `datatype` are treated as single words.
 - The `i` prefix is used to designated `infered`.

Where these naming conventions have not been used consistently, this should be
considered a bug.
