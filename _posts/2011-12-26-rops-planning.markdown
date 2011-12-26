---
layout: post
title: Planning a Scheme 
tags: scheme rops
---

In my [previous post](/2011/12/23/scheme-as-a-rite-of-passage.html), I
stated I was going to build yet another toy Scheme implementation.
Despite I still do not know every detail of it yet, I do have a
general idea of where I want to go, so I will try to sketch and
motivate my plan in this post. These are the main points:

- It will be a toy implementation, for pedagogical purposes only
  (actually, mainly to teach *myself* about PLT). Do not ever use it
  in production. It may set your computer on fire.
- I will start with an interpreter rather than a compiler.
- It will be a subset of Scheme, but I want it to be a _meaningful_
  one. For example, I want it to support continuations and macros,
  although I will restrict the basic data types to booleans,
  (machine-dependent) integers and strings.
- I want it to be experimental and hacker-friendly, so I will try to
  keep the code as modular as possible (so you can, for example,
  change the reader without affecting the evaluator). I will host the
  code at github and make extensive use of branching.

Snarf, snarf!
-------------

A scheme interpreter is obviously just a program and, as such, written
in some _host_ language. This means you can add features to your toy
Scheme by "stealing" them from the host language instead of
implementing them explicitly. For example, if you implement your
interpreter in a garbage-collected language, you can easily avoid
writing your own garbage collector: you just need to think of a proper
mapping between the objects in scheme and the data structures managed
by the host language. This technique is known as _snarfing_ and is of
course limited by the number of helpful features offered by your
language of choice. Some of the things that make implementing a Scheme
easier are:

- First class closures.
- Tail-call optimization.
- Continuations.
- Garbage collection.
- A syntax made up of Symbolic expressions.

You can imagine two limits depending on how many features your host
language provides. On the lower end there is assembly (or C), with
none of the above, which would force an explicit implementation of
everything. On the other end of the line there is Scheme itself, which
would lead us to the so-called _meta-circular_ implementation. Most
high level languages lie somewhere in-between those two limits.

One of the most important decisions to make when implementing a Scheme
interpreter is, then, what language to use and how much to snarf from
it. Also note that the fact that you _can_ snarf a feature does not
mean you _have_ to. I'd like to leverage that: starting with a snarfed
feature and then change it to an explicit implementation, for example. 

The language
------------

_DISCLAIMER_: language wars bore me. The choice of the host language
will be a matter of my current interests and personal taste. I will
try to motivate it a little bit, though, because I really think it
will be a _good tool for the job at hand_.

First of all, I am not going to use _meta-circularity_. I always felt
it's easy to miss where the _meta_ level starts if you use the same
language everywhere. I think the explanation will be clearer if
the host language is radically different from Scheme. Apart from that,
I wanted to write something in a statically-typed, functional language
(my skills are getting a bit rusty on this area lately).

So I am going to use [Objective Caml](http://caml.inria.fr/). 

OCaml is a statically-typed, multi-paradigm language with a heavy
functional bias. It allows for mutable state and falling back to
imperative style seamlessly in your code (contrast that with Haskell,
where impure parts have to be isolated). I'm not saying this has to be
_good_ per se, just that it is easier to implement a Scheme with those
primitives (eg. the environment data structures are easier to
implement if they are mutable).

OCaml has good tools for language implementation: the default
distribution comes with lex and yacc-style tools, and there is also a
super-preprocessor, CamlP4, that lets you extend OCaml's syntax and
can also be used to develop new languages. As a lisp-biased hacker, I
want to know how this feels in contrast to lisp macros (I already know
the theory, I want to know how it _feels_).

OCaml supports object-orientation, though I think I will stick to
algebraic data types for the evaluator: the implementation will be
more concise and the code in one place.

OCaml is available for Linux, Windows and Mac Os X, compiling
both to bytecode and to optimized native code. And it has a decent
_repl_, which makes everything easier (I find myself hardly ever using
the debugger when a language has a repl).

I think most people familiar with statically-typed functional
languages will have no trouble in following the explanations, so
people may follow along in Scala or Haskell or F# (Microsoft's
descendant of OCaml), or whatever...

The Process
-----------

I'll take an incremental approach, starting with barely more than a
polish notation calculator and then adding feature by feature. At the
end of each post one should have a working _something_. I expect the
roadmap to be along the lines of:

1. Building a Polish notation calculator on steroids. Just enough to
define the general structure and to deal with parsing.
2. Add an _environment_ so that you can assign variables.
3. Adding special forms for closures.
4. Adding continuations.
5. Adding macros.

I may intertwine more theoretical posts whenever I think they're
relevant (in my experience, most programmers are not familiar with
continuation-related topics, for example). I may also branch two
implementations of the same feature (snarfing TCO vs explicitly
implementing a stack for continuations, or using CamlP4 instead of
ocamlyacc), and digress into some OCaml or Scheme-specific aspect in
the process.

Code will be hosted at
[my github repo](https://github.com/jarnaldich/rops). Development will
always be a little ahead from blogging, and each blog article will
have its corresponding branch (in a perfect world they would be tags,
but since there will be bugs, I'd rather let them evolve).

Hope this will be a lot of fun...
