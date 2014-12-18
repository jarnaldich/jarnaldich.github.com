# Joan's Cozy Common Lisp #

Like many other languages, Common Lisp has its own equivalent of a package  manager called [Quicklisp](http://quicklisp.org) available. But, unlike in most other languages, it is not only _libraries_ that you can download and install. Honoring its _programmable programming language_ nature, QuickLisp lets you download and install _systems_ that can affect the developer's programming experience in a way only Lisps can do: you can extend syntax constructs with (reader-)macros, up to define whole new DSLs for specific tasks. Having such a mechanism is even more important in Common Lisp than in other Lisps: as many parts of Common Lisp get "frozen" by the ISO standard, coders started to fill the gaps needed for modern, real-world programming through user-level systems they can now share on QuickLisp.

Here is a list of systems I use so often I feel they are part of my "custom" Common Lisp. In non-Lisp lingo, I don´t think of them as "libraries", but rather as core language features: it just happens to be my own  _Cozy Common Lisp_.

## Iterate
Let's face it: the loop macro is ugly. It is very powerful, but it is not very intuitive and it embeds a non-lispy DSL that most editors have a hard time at formatting. It has a lot of corner cases and keywords and it´s easy to abuse.

Just compare the examles from Iterate's home page. We are trying to find the longes list in a list of lists. The loop version:
```lisp
(loop with max-elt = nil
      with max-key = 0
      for elt in list-of-lists
      for key = (length elt) do
      (when (> key max-key)
        (setq max-elt elt
              max-key key))
      finally (return max-elt))

```
The iterate version:
```lisp
(iterate (for elt in list-of-lists)
         (finding elt maximizing (length elt)))
```

Furthermore, Iterate is designed to be [extensible](http://common-lisp.net/project/iterate/doc/Rolling-Your-Own.html#Rolling-Your-Own) from the beginning.

metabang-bind
cl-anonfun

cl-fad
alexandria

(asdf-install? quicklisp?)
