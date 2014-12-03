# On JavaScript #

I remember playing around with JavaScript back in the times of
Netscape Navigator. It wasn't supposed to have a bright future back
then (anyone remember Java Applets?), so after the first few WTFs
(pick your favourites: [wtfjs](http://wtfjs.com/)), I soon ran away in
fear and never looked back at it for more than fifteen years.

Web programming has changed a lot since and, to the astonishment
of many, JavaScript has become the most pervasive technology in this
revolution. And while it is true that
[JavaScript implementations](https://code.google.com/p/v8/) have
improved dramatically, all this explosion happened without the
language itself getting any better.

For a hobby project of mine involving 3D programming for the web, I've
been forced back to JavaScript, so I decided to look at it more
closely. I happily found out that another area in which JavaScript has
improved are learning materials. In particular, I found the work of
[Douglas Crockford](http://en.wikipedia.org/wiki/Douglas_Crockford)
enlightening.

If you are planning to start some serious coding in JavaScript, please
do not write a single line of code until you watch this video:

<!-- <iframe width="420" height="315" src="//www.youtube.com/embed/lP9-Zx_cCUg?rel=0" frameborder="0" allowfullscreen></iframe> -->

And, of course, getting a copy of the book is pretty much a must.

## Scheme in disguise ##

One of the main points of the talk is Douglas' epiphany that
JavaScript was meant to be more like
[Scheme](http://en.wikipedia.org/wiki/Scheme_(programming_language))
than like Java. As a long-term Scheme fan, this should have sort of
cheered me up, but it didn't... I can only take it as just another
proof of how far people want to stay away from Lisp: they'd rather
accept a half-baked, underspecified, implementation of a curly brace
language than accept a mature language with an open standarization
process and a whole lot more power just because its surface syntax
looks funny.

The sad thing is, it is not just a matter of parentheses:
[Lua](http://www.lua.org) is a tiny language with an algol-like syntax
that could serve the same purposes JavaScript was invented for, and
that has efficient [JIT compilation](http://www.luajit.org) and
[macros](http://metalua.luaforge.net/). Oh! And it also gets
[prototype-based inheritance](http://www.lua.org/pil/16.1.html) right.
Take this as a proof that we *could* have gotten a better JavaScript.

## JavaScript as a Virtual Machine ##

Luckily, Javascript's problems are well described, and the speed it
gets in modern browsers make it possible to use it as a *target
platform* for compilation.

Thus, a wealth of languages that compile down to Javascript have sprung
in the past years. The range varies from those which just try to address
its [syntactic quirks](http://coffeescript.org/) to ones with
[whole new semantics](http://elm-lang.org/). There are also versions
of server-side languages compiled to javascript
([clojure](https://github.com/clojure/clojurescript),
[Haskell](https://github.com/faylang/fay/wiki) has
[many](https://github.com/ghcjs/ghcjs)
[options](http://haste-lang.org/)), which in theory helps blurring the
separation of tiers.

There is even an
[LLVM backend](https://github.com/kripken/emscripten), so that C code
can be run on your browser with no modifications...

Again, all these languages will not let you completely forget there is
something called Javascript down there. In any respectable sized
project you will indeed need some kind of Javascript FFI, and a good
share of more or less ugly plumbing for integration.

## JavaScript, the Community ##

And yet, it is hard not to get impressed by the utterly awesome open-source
libraries and applications written in Javascript. The browser as a
platform is one of the areas in computing that has most radically
changed in the last years (along with GPUs and the Cloud), mainly due
the V8 and HTML5.

And that is undoubtedly a merit of its community. There is one thing
Javascript got right: the source is right there. You may minify it,
obfuscate it or whatever, but the fact is that it is much easier to
publish the code as-is than to disguise it, and this very fact
fostered a culture of openness that empowered users and develpers as
well. Commercial companies soon realized that the way to go was not
taking pains to hide the source, but publishing their code as a way to
demonstrate the quality of their code.

Take this as a proof of what the world would be like if open source
was the rule, not the exception. 

## Conclusion (sort of) ##

* We programmers are pretty good at making the most inconvenient
  technologies succeed.
* **Never hire a JavaScript developer that does not know what a
  closure is.**
