---
layout: post
title: Planning a Scheme 
tags: scheme rops
---

In my [previous post](/2011/12/23/scheme-as-a-rite-of-passage.html), I stated I
am going to build yet another toy scheme implementation. Despite I still do not
know every detail of it yet, I do have a general idea of where I want to go,
so I will try to sketch and motivate my plan in this post. These are the main points:

- It will be a toy implementation, for pedagogical purposes only (actually,
  mainly to teach *myself* about PLT). Do not ever use it in production. It may set your computer on fire. 
- I will start with an interpreter rather than a compiler. 
- It will be a subset of Scheme, but I want it to be a _meaningful_ one. For example, I want it to support continuations and macros, although I will restrict the basic data types to booleans, (machine-dependent) integers and strings. 
- I want it to be experimental and hacker-friendly, so I will try to keep the code as modular as possible (so you can, for example, change the reader without affecting the evaluator). I will host the code at github and make extensive use of branching.

##Snarf, snarf!

A scheme interpreter is, obviously, just a program and, as such, implemented in
a programming language: the so-called _host_ language. You can therefore
implement some features of Scheme by "stealing" them from the host language.
This is known as _snarfing_, and is limited by the features
supported by the host language.  For example, if you implement your interpreter
in a garbage-collected language, you can rely on the underlying garbage
collector to manage scheme objects.


## The repo
