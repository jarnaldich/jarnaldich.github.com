# Joan's Cozy Common Lisp #

Like many other languages, Common Lisp has its own equivalent of a package  manager called [Quicklisp](http://quicklisp.org) available. But, unlike in most other languages, it is not only _libraries_ that you can download and install. Honoring its _programmable programming language_ nature, QuickLisp lets you download and install _systems_ that can affect the developer's programming experience in a way only Lisps can do: you can extend syntax constructs with (reader-)macros, up to define whole new DSLs for specific tasks. Having such a mechanism is even more important in Common Lisp than in other Lisps: as many parts of Common Lisp get "frozen" by the ISO standard, coders started to fill the gaps needed for modern, real-world programming through user-level systems we can now share on QuickLisp.

Here is a list of systems I use so often I feel they are part of my "custom" Common Lisp. In non-Lisp lingo, I don´t think of them as "libraries", but rather as core language features: it just happens to be my own  _Cozy Common Lisp_.

alexandria
iterate
metabang-bind
cl-fad

(asdf-install? quicklisp?)
