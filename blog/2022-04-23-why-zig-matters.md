---
title: Why Zig Matters
date: 2022-04-23T00:00:00
tags: zig, C, gdal
---

... or at least why I think it does.

On a [recent article](https://gankra.github.io/blah/c-isnt-a-language/) worth
reading, Aria Bingessner makes her point about C not being a language anymore,
but (through its ill-defined ABI) the _lingua franca_ of programming.

I used to like C. Undefined behaviours did not bother me much because most of
the times I was able to control the environment in which my code ran in terms
of compiler, architecture and OS. Or at least that is what I used to tell 
myself, because, while it is true that do not bother me _while writing code_,
one of the consequences of these behaviours was one of the main reasons I would try to avoid it
in a modern project involving systems programming:
the very fact that writing portable code is so hard makes having a package
manager especially difficult (that is why there are training videos on
[conan](https://academy.jfrog.com/path/conan) but you can figure out
[cargo](https://doc.rust-lang.org/cargo/guide/dependencies.html)
for dependency management in minutes). For me, modern systems programming
languages like [Rust](https://www.rust-lang.org/), [Go](https://go.dev/) or [Swift](https://www.swift.org/)
are not that much about the language itself, but being able to easily share code
and build on other people's code.

Still, using any of these languages means having to rely on C's ABI (with all
the trouble described in Gankra's article). That is someone else's problem if
you stay within the language itself (eg. use "pure" packages), but my bet is you
will be affected by it in some way in a non trivial project involving
integration.

But, what if there was a language designed not to replace C, but to live along
with it, with an integrated build system designed to directly (cross-) compile
C? What if you could start using your favourite C libraries (almost) seamlessly
and then decide wether to _gradually_ improve on them in terms of safety and
ergonomics?

## Show me some code!

I just started my journey with Zig, but it already helped scratch some itch I
had with the [Geospatial Data Abstraction Library](https://gdal.org/). For those
of you not in the geospatial world, it's the library lying at the core of most
Geospatial Open Source software, and is also used by the main commercial actors. 
Most people use it via third party software, via it's command-line tools or
via some other language's interface (basically Python's).

Now, from time to time you reach a point where you need a function that is not
easy accessible via the command-line executables and need to fall back to a
python script for that. Depending on how you want to deploy that you then have
to make sure your users have an appropiate Python environment. For some small
utilities that is just overkill. Eg, on windows, I'd rather hastily compile an
.exe and copy-deploy to the users, who should just care about running it
as any other native executable (deploying Python applications to end users can
be less than fun).

## The build script

We could, in theory, use zig to compile the whole GDAL library, and that would
certainly [make the most
sense](https://zig.news/kristoff/compile-a-c-c-project-with-zig-368j) but for a
first project that is overkill. We will for the moment rely on having the library available in our system. 

Starting a project with `zig init-exe` will automatically create the file
`build.zig` that is the equivalent of a makefile (or rather, a combination of
CMake, configure ... and make). The cool thing is that it is also just a zig program




## Conclusion

In my (begginer's) opinion, Zig proposition of value is unique and much needed,
so I really hope the language succeeds. That said, the competition in the system's
programming language space is *fierce*, with all the major players (Google,
Apple, Amazon) taking sides and pushing their own, so I hope it's technical
features will grant a place for it in the long run.

