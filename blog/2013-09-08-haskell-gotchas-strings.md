---
title: "Haskell Gotchas: Strings"
date: "2013-09-09T00:00:00"
tags: haskell, gotchas, strings
---

I've been playing with [Haskell](http://www.haskell.org) on and off
for some time now. Let's face it: it's not an easy language to learn.
I would agree with most people that part of the difficulty comes from
its unique features (laziness, purity, etc...), and the abstractions
that derive from them when facing real-world problems (Monads,
Iteratees, etc...), but my point here is that there are other sources
of confusion that keep misleading beginners like myself and that have
nothing to do with all that.

<!-- more -->

In fact, I can see a pattern in those
[gotchas](http://catb.org/~esr/jargon/html/G/gotcha.html): Haskell
often provides several tools for a particular task. It is (too) often
the case that the one that's most at hand, or that seems to be favored
in the docs, is just not what a newbie would expect. I think this fact
has an impact on the perception of first-time users, if they do not
perservere enough to seek for alternatives.

I am planning to write a series of posts on these gotchas, if only to
prevent myself from falling into the same traps the next time I decide
to take on Haskell again, and in the hope they will be useful to other
learners. Please mind that I have no valorative intetion whatsoever
(no flames: it is not a Haskell WTF), and that this is not the work of
a seasoned Haskeller: if you find any mistakes in these posts, please
report so I can correct them and learn something.

# Strings

This is a paradigmatic case, and probably the reason for many newbies
walking away from Haskell wondering how come a compiled language can
perform [so much worse](http://honza.ca/2012/10/haskell-strings) than,
say, Python.

To recap, Haskell has a `String` type. Now, for any non-trivial text
processing you can pretty much forget about it. In Haskell, the
`String` type is defined as a regular list of `Char`, and if you've
had some previous exposure to functional lists (with their `car`s and
`cdr`s, `head`s and `tail`s, ...) you'll know how different a beast
they are from the sort of `char`/`wchar` array most newcomers would
expect for a string implementation.

Of course, Haskell ships with a bunch of modules designed to work as
`String` replacements in its standard library. By replacement I mean
that the names of the functions are kept consistent so that, if you
use qualified imports, it _should, in theory_ be easy to switch between
them. Now, these modules not only change the underlying implementation
of a string, but also provide the functions to perform IO according to
the type, so they come in _strict IO_ and _lazy IO_ flavors: the
_gotcha_ here is that this can dramatically change the _semantics_ of
input/output on the importing module, so switching between them is not
always that easy, _in practice_.

I have deribelately avoided to tackle the subtleties of lazy IO in
this post (I may keep that for another gotcha). Take a look at
[this](http://www.haskell.org/haskellwiki/Iteratee_I/O#The_problem_with_lazy_I.2FO)
if you can't wait. At the moment, my advice for a newcomer would be to
start with the strict versions, because they are closer to the
behaviour you'd expect in most other languages.

If you have already been doing IO in Haskell with `Strings` and
`System.IO`, then you have already been doing _lazy_ IO, since it's
the default. When in doubt, you can always try both and see which one
(if any) matches your performance expectations.

## The Basics

Here's what most Haskellers would recommend:

**If you do not care about Unicode**, use `Data.ByteString.Char8`,
which is a packed array of `Char8` (bytes). The lazy variant is
`Data.ByteString.Lazy.Char8`. This will be enough if you can assume
your input is in (a subset of) _latin-1_. 

```haskell
import qualified Data.ByteString.Char8 as B
```
or

```haskell
import qualified Data.ByteString.Lazy.Char8 as B
```

**If you care about Unicode**, go use `Data.Text.Text`:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
```

or

```haskell
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
```


If you need regular expressions with Unicode, though, the thing gets a
little more [involved](http://stackoverflow.com/questions/14922579/haskell-regular-expressions-and-data-text).

## Back and forth

Even if you use these types, you will still need `Prelude.String` in
your code: there are a lot of libraries which will expect and return
`String`s. As an example, the `FilePath` type for file and directory
manipulation is just an alias for `String`. Also, every string literal
in your code will be parsed as a `String` by default (but see below),
so converting from _packed_ `ByteArrays` to _unpacked_ `String`s is
achived, not surprisingly, by the functions `pack` and `unpack`. In
fact, using `String` in your APIs, as long as they're not too large,
is one (the only?) sensible use for `Strings`.

For the [GHC](http://www.haskell.org/ghc/) stack you can 
avoid packing and unpacking string literals by using the
`OverloadedStrings` pragma. Ie. instead of writing:

```haskell
import qualified Data.Text as T
myFuncExpectingDataText . T.pack $ "Hello World!"
```

you can add the pragma that makes the call to `T.pack` unnecessary:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
myFuncExpectingDataText "Hello World!"
```


## Conclusions

Here's my piece of advice:

- Avoid `Prelude.String` for text processing, but bear in mind it is sort
  of standard when defining your APIs.
- If you care for Unicode, go for `Data.Text`, if the latin-1 subset
  is enough for you, stick to `Data.ByteString`, since regular
  expressions (and other tasks) are easier there.
- If you are starting and the performance is enough for your use-case,
  go for the strict IO. The behaviour is more predictable.

A final note: this is, by far, not the last word regarding `String`s
in Haskell. For example, there are abstractions that aim to solve the
predictability issues problems of lazy IO while keeping performant
(for example,
[Iteratees](http://www.haskell.org/haskellwiki/Iteratee_I/O) or
[Conduits](http://www.haskell.org/haskellwiki/Conduit). I just think
this is the bare minimum to be able to do text-processing in Haskell.
