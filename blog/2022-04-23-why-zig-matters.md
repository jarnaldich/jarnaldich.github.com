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
are not that much about the language itself, but being able to easily share and
reuse code.

Still, using any of these languages means having to rely on C's ABI (with all
the trouble described in Gankra's article). That is someone else's problem if
you stay within the language itself (eg. use "pure" packages), but my bet is you
will be affected by it in some way in a non trivial project.

But, what if there was a language designed not to replace C, but to live along
with it, with an integrated build system designed to directly (cross-)compile
C? What if you could start using your favourite C libraries (almost) seamlessly
and then decide wether to _gradually_ improve on them in terms of safety and
ergonomics? 

Well, enter [zig](https://ziglang.org).

## Show me some code!

I just started my journey with Zig, but it already helped scratch some itch I
had with the [Geospatial Data Abstraction Library](https://gdal.org/). For those
of you not in the geospatial world, it's the library lying at the core of most
Geospatial Open Source software, and is also used by the main commercial actors. 
Most people use it via third party software, via it's command-line tools or
via some other language's interface (mostly Python's).

Now, from time to time, you need a function that is not
easy accessible via the command-line executables and need to fall back to a
python script, which immediately bears the problem of providing a
suitable python environment to the end user, or packing a Python self-contained
app. Both options can be less then fun, and for small
utilities that is just overkill compared to, for example, hastily compiling a
native executable and copy-deploy it.

## Download and install

To start using `zig` you just need to download the latest relase from
[here](https://ziglang.org/download/). It is a small (around 40Mb), self
contained release for many platforms. Once you download that you can enter the
directory of your like and scaffold your very first application by entering `zig
init-exe`. That will create the build script `build.zig` and a trivial main file
under `src\main.zig`. You can then build it with `zig build` and execute the
result from `zig-out\bin\`. That is really all it takes.

Of course, if you want the full experience, you will also need an editor with
syntax highlighting or whatever. Head
[here](https://ziglang.org/learn/getting-started/#recommended-tools) and choose
your poison.

## The build script

We could, in theory, use zig to compile the whole GDAL library, and that would
certainly [make the most
sense](https://zig.news/kristoff/compile-a-c-c-project-with-zig-368j) but for a
first project that is overkill. We will for the moment rely on having the
library available in our system.

Starting a project with `zig init-exe` will automatically create the file
`build.zig` that is the equivalent of a makefile (or rather, a combination of
CMake, configure and make). The cool thing is that it is also just a zig
program. Here's a snippet of what it means to link with an existing library
(just the paragraphs starting with a comment, the rest is included for context):

```zig
  const exe = b.addExecutable("<name>", "src/main.zig");
    exe.setTarget(target);

    // GDAL needs to link libc
    exe.linkLibC();

    // Just if not made available otherwise
    exe.addIncludeDir("<...>");
    exe.addLibPath("<...>");

    // Add to the link
    exe.linkSystemLibraryName("gdal"); // or gdal

    exe.setBuildMode(mode);
    exe.strip = true;
    deps.addAllTo(exe);
    exe.install();
```

And whenever we build the project with `zig build` it will link against `gdal`
now. Now, to actually use the library in our code, we just need to import the C
header file directly with:

```zig
const gdal = @cImport({
    @cInclude("gdal.h");
});
```

... Take a moment to process that: the header is parsed at compile-time through
the Clang toolchain, which means it understands preprocessor directives,
conditional compilation, constants... and is made available to us so that we can
just call:

```zig
gdal.GDALAllRegister();

const hDataSet = gdal.GDALOpen("<sample>", gdal.GA_ReadOnly);

```

which is basically [all you would
do](https://gdal.org/tutorials/raster_api_tut.html) to open a dataset in C, too.

Now, we get to play around a bit with what is being done here. Zig is strongly
typed, but has automatic type inference, so we didn't care to declare them. One
cool feature of Zig is its ability to run code at compile time (for C users,
imagine having a preprocessor that is C itself; for lisp users: you already know
that stuff). We can treat types as values at compilation time, which means adding this:

```zig
std.debug.print("{s}\n", .{@typeName(@TypeOf(gdal.GDALOpen))});
```

will print the type signature of the function (I guess we would know if using LSP, but
just to show Zig introspection capabilities):

```
fn([*c]const u8, c_uint) callconv(.C) ?*anyopaque
```

That means Zig will treat `gdal.GDALOpen` as a function that:

- Takes a `[*c]const u8`, which is zig's way to declare a `const char*`.
- ...and an integer of whatever size is the default in our architecture for clang:
  `c_uint`. Bear in mind that we used the call as `gdal.GA_ReadOnly`, which
  means zig got the `enum` values right for us.
- Returns an opaque pointer that could be null `?*anyopaque`. That is zig's
  equivalent of a `void*`.
  
So, not surprisingly, what we get out of the box is more or less the same
"safety" we would get using C.

We can easily improve on that for enhanced security and ergonomics, but _we do
not have to_. In any case, I will develop that further in a follow up post. This
was just intended to whet your appetite on what it is possible with Zig.

## Conclusion

In my (begginer's) opinion, Zig proposition of value is unique and much needed,
so I really hope the language succeeds. That said, the competition in the system's
programming language space is *fierce*, with all the major players (Google,
Apple, Amazon) taking sides and pushing their own so, aside from the technical
goodies of the language, taking it to 1.0 will require strategic vision and
smart community and project management (again, just dabbling with it, but the
work on this areas so far seems to have been equally great, so...).

## Further reading

- https://kristoff.it/blog/maintain-it-with-zig/
