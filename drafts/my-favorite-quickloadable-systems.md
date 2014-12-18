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

# metabang-bind
Common Lisp has many binding constructs. If you need to combine more than one in the same piece of code you can end up with a lot of spurious indentation levels. For example, if you need to use a locally-defined function, destructure a list and retrieve the result of a multiple-value returning function, you would do:

```lisp
(destructuring-bind (a b) '(1 2)
  (multiple-value-bind (c d) (values 3 4)
    (flet ((plus-one (x) (1+ x)))
      (let ((l (list a b c d)))
        (mapcar #'plus-one l)))))
```

The package [metabang-bind](http://common-lisp.net/project/metabang-bind/user-guide.html) was designed to solve that problem. It offers a single construct and a well-designed syntax that will let you write the above example as:

```lisp
(bind (((a b) '(1 2))
       ((:values c d) (values 3 4))
       ((:flet plus-one (x)) (1+ x))
       (l (list a b c d)))
  (mapcar #'plus-one l))
```
I certainly prefer the latter (I can understand people prefering the former, though, since it makes the scope of every definition explicit).

The bind macro also lets you destructure objects, tructs, bind the result of regular expressions and other goodies. And again, it is extensible. Check the [manual](http://common-lisp.net/project/metabang-bind/user-guide.html).


metabang-bind
cl-anonfun

cl-fad
alexandria

(asdf-install? quicklisp?)
