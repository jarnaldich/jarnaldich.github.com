---
layout: post
title: Raw Strings in Factor 
tags: factor concatenative
---


Ok, you may think I have a fixation with [raw
strings](2011/08/07/raw-strings-in-racket.html). If you're like
most developers and have used different languages through the years,
you have probably experienced the feeling of wanting to port your
favourite features from language to language.

Most languages just won't let you do anything about it, but then,
happily, there is also a whole family of _programmable programming
languages_: [Lisps](http://www.paulgraham.com/chameleon.html) being the
most notable members among them, there are, for sure, other languages
in other sometimes almost forgotten paradigms.

## Concatenative

[Factor](http://factorcode.org) is a so-called
[concatenative](http://concatenative.org/wiki/view/Concatenative%20language)
language. More precisely, it a
[stack-based one](http://c2.com/cgi/wiki?StackBasedLanguage). What
sets it apart from other concatenative languages IMHO is that it's a
_general purpose_ (Forth is more oriented towards embedded systems)
_practical_ (not just a theoretical tool) and _modern_: its creator
Slava Pestov and the development team have brilliantly shown how
object orientation, incremental compilation, and bunch of advanced
language features can be implemented in a concatenative language.

## Factor as a shell

Factor has a very terse syntax. This can be good or bad, depending on
your application and the way your brain is wired up. Lately, I've
started using its
[visual repl](http://re-factor.blogspot.com.es/2010/09/visual-repl.html)
as an os shell on steroids (I'll eventually blog on the experience). I
think it makes sense since I spend a good share of my time on Windows
and I'm not crazy about the syntax of Powershell. A shell is clearly
one of the applications where less typing is just the right thing, and
the stack-based model sometimes feels like unix piping on steroids (
I'm thinking of the
[dataflow combinators](http://docs.factorcode.org/content/article-dataflow-combinators.html)
) here.

## Raw strings

And, of course, being able to shamelessly use backslashes inside
strings is something you ask of a windows shell. I want to be able to
type something like:

{% highlight factor %}
r| \\Server\share| directory-files
{% endhighlight %}

Note the space after the first vertical bar. This is typical of
factor, you'll see why in a moment.

To get it, let's RTFM:

> The Factor parser follows a simple recursive-descent design. The parser reads successive tokens from the input; if the token identifies a number or an ordinary word, it is added to an accumulator vector. Otherwise if the token identifies a parsing word, the parsing word is executed immediately.

So it looks like we'll need to introduce a word that will hook up to a
function whose responsibility will be to push a string into the
accumulator. Something like:

{% highlight factor %}
SYNTAX: r| 124 parsing-raw ;
{% endhighlight %}

This means that the parser will inmediately evaluate `124 parsing-raw`
after seeing the introductory `r|`. since this introductory word is
handled by the default lexer and parser, there needs to be a space
after it for factor to process the end of word. This might seem
unnatural first, but it is consistent with all such factor extensions.



### The reader


## Resources

- Slava's post on
  [writing DSLs on factor](http://factor-language.blogspot.com.es/2009/09/survey-of-domain-specific-languages-in.html)
  gives a nice overview of factor's self-modifying capabilities.

