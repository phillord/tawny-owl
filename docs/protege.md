Tawny with Protege
==================

While Tawny allows construction of any OWL ontology, it has a programmatic
interface. By itself, it does not provide strong support for visualisation of
ontologies; this is something that [Protege](http://protege.stanford.edu) is
very good at. Here we describe three ways to integrate with Protege, using it
as a visualisation framework.

Caveat
------

First, we start with an entirely negative point. All the uses of Protege that
we describe here are good only for displaying the _generated_ OWL produced by
Tawny. As tawny can be to build arbitrary abstractions on top of OWL, and is
fully programmatic, this may or may not be a useful thing to do. Trivially,
for example:

    (doseq [k (range 100000)]
        (owl-class (str k)))

will produce a very large ontology from a very small statement. This also
means that Protege is *read-only* -- changes made in Protege will not affect
the Tawny source. Our experience is that Protege is actually fairly useful as
a read-only viewer.

Caveat 2
--------

At the time of writing, the current release version of Protege uses a
different version of the OWL API. So inconsistencies between the two are
possible. For our use, we have developed against a development version of
Protege patched to use the same version of the OWL API.

Tawny is likely to remain closely synced to releases of the OWL API, rather
than Protege, so this may always remain a problem. From a development point of
view, building Protege from source is relatively straight-forward, but this
might be an excessive demand for "normal" users (and if you know of any,
please tell me!).

File-Based Access
-----------------

The easiest way to use Protege with Tawny is simply save the ontology to a
file. This example from `tawny.pizza` shows how to add this to a main
function, which will run with `lein run`.

    (ns pizza.core
      (:use [tawny.owl]
            [pizza.pizza]
            [pizza.render-pizza])
      (:gen-class))

    (defn -main [& args]
      (with-ontology pizzaontology
        (save-ontology "pizza.rdf" :rdf)
        (save-ontology "pizza.owl" :owl)))

Now, `pizza.owl` can be opened in Protege. It will detect when the files
have updated, and offer a reload. This workflow is okay, but does not provide
a rich interaction between tawny and Protege; in particular, effectively
resets the GUI, leaving which is painful.

Tawny in Protege
----------------

And second approach is to use Tawny inside Protege.
[Protege-Tawny](https://github.com/phillord/protege-tawny) provides a console
and REPL inside Protege. It is possible to type commands that have immediate
affect, on the Protege GUI.

While this works, it essentially offers the opportunity to use Tawny as an
OPPL like language for adding new terms to an existing ontology. The source is
not saved, and not available next time around.

Protege as an nREPL Server
--------------------------

It is also possible to use Protege as
[nREPL server](https://github.com/phillord/protege-nrepl). Essentially, this
means instead of launching a REPL using your IDE, you launch Protege and then
connect to it with your IDE.

This provides the tightest integration with Protege; Tawny code can be
evaluated as normal, and any changes appear immediately in Protege.
Preliminary integration with Emacs is also provided, enabling "Jump to" and
"Tracking" functionality. Currently this an early release (as it requires
building Protege from source), but our hope is this becomes stable and easy to
use in the future.
