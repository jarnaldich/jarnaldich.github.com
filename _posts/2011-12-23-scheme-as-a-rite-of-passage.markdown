---
layout: post
title: Scheme as a Rite of Passage 
tags: scheme rops
---

##The Theory 

Some days ago the [Greenspuns tenth rule of
programming](http://c2.com/cgi/wiki?GreenspunsTenthRuleOfProgramming) came back
to my mind for some reason.  I have myself come up with quite a
lot of such rules over the years, which I will eventually write down in a further
post. One of the most notable among them is, no doubt, the following theorem:

> Given enough time, all Scheme coders will try to implement their own
> stripped-down, ill-defined, non-standard and buggy Scheme interpreter.

The proof is left as an exercise to the reader (Hint: use induction. Take
Chapter 4 of [The
Wizard Book](http://mitpress.mit.edu/sicp/full-text/book/book.html) as the base
case, and a mix of programmer naivety and hybris as the inductive step). I like
to call this theorem _Scheme as a Rite of Passage_.

##The Practice

But wait, it's actually worse than that, because it is no longer Schemers only
who will try to build and ride their own bicycle, but also almost anyone
interested in programming language theory... writing your own lisp has become the *Hello
World* of language theory.

I can't help thinking we are all giving Scheme a bad name by saturating the net's
code repos with those toy implementations, specially when authors don't always
clearly state what kind of interpreter/compiler they are offering: it takes
less than a week for an average hacker to roll such a Scheme, but it can take a
group of expert *years* to write a high-quality Scheme that *performs well*. 

##The Crime

It would probably make much more sense to spend some time studying the inners
of an already existing implementation rather than spanning new
projects over and over again, right?

And yet, and yet...

The Right of Passage seems to be an almost essential part of the learning
process. It is difficult to understand the code in other projects unless you
have actually tried yourself before.  At some point,
one just needs to undergo this rite of studying different implementation
strategies and dealing with the practical issues that arise along the way. 

Anyway, in the process of implementation you will reach a deeper insight
of how Scheme, but also programming languages in general, work. So, even if you do
not succeed in having something
fully working, it will make you a better coder.

That's why I call it a rite of passage: one does not care if it's been done
thousands of times before, or if there's plenty of code already publicly
available (most of it much better than the one about to be written), because it
is the _process_ that matters.

So here I go: I'm planning to start a _not-to-be-used_ Scheme implementation and to blog
about it as often as my job and a 5 month old daughter let me (take that as my
new year's resolution). I also expect to digress from it into other more-or-less theoretical CS topics. 

##The Source

Before getting one hand's dirty, it's just a good practice to see what other's
have done before. I mean, there are plenty of fine-working wheels out
there. Below is a list of the sources I have found useful so far... but beware that,
for the right of passage to actually work, you must:

1. Go through these sources.
2. Forget about them. 

Number 2 is important: you must be capable of wrapping some big parens (pun
intended) around what you have read. Just enough to make your own mistakes, but
not as much as to make an idiot of yourself. Your implementation must
be indeed your own: sticking too much to other people's material is considered
cheating, and just not worth publishing anything about (not even crappy blog posts).

I will expand the list below as I work through the interpreter. If you know of
any other material that could be useful, please comment this post. Everything
listed below is publicly available, but some of the materials are also
 for sale. If you happen to like them, please support the authors.

- The aforementioned meta-circular evaluator in [The Wizard Book](http://mitpress.mit.edu/sicp/full-text/book/book.html).
- A Wikibook on implementing a scheme in Haskell: [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours). Does not include continuations. I like it mostly as an excuse to learn Haskell and Parsec.
- [Exploring Programming Language Architecture in Perl](http://billhails.net/Book/) builds an interpreter for a scheme dialect in Perl, and then implements threads, logic programming, and _amb_ on top of it. Includes continuations. One of my favourite.
- [Norvig's article](http://norvig.com/lispy.html) on implementing a tiny lisp dialect in Python. A toy, but very concise and easy to understand. 
- [An Introduction to Scheme and its Implementation](ftp://ftp.cs.utexas.edu/pub/garbage/cs345/schintro-v14/schintro_toc.html) works if you don't know Scheme yet. The text is a bit rough, but useful in spite of being in a `pub/garbage` directory.
- [Scheme 9 from Empty Space](http://www.e-booksdirectory.com/details.php?ebook=4625) by Nils M Holm covers the implementation of a Scheme interpreter in C. Since the target language is C, it implements its own garbage collector.

The above cover interpreters. If you are interested in implementing a compiler there's:

- [Three Implementation Models for Scheme](www.cs.unm.edu/~williams/cs491/three-imp.pdf)  by RK Dybvig is a classical paper. I think only the two first models are still relevant. The stack-based model is the inspiration for a recent javascript interpreter called [BiwaScheme](http://www.biwascheme.org/). Beware: it compiles to an intermediate language, *not* assembly.
- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) brings native-code compilation to the masses.

Other resources:

- Peter Michaux 
  [blogged about](http://michaux.ca/articles/scheme-from-scratch-introduction)
  his own rite of passage.
- This [Reddit thread](http://www.reddit.com/r/scheme/comments/fvbjr/question_which_scheme_implementation_has_a_better/) on easy-to-understand implementations.
- The [ReadScheme](http://readscheme.org/) papers. It's probably easy to get lost here unless you are a researcher...
- And, of course, the [Schemers](http://www.schemers.org) website.
