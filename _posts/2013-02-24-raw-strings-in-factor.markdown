---
layout: post
title: Raw Strings in Factor 
tags: Factor concatenative
---

Ok, you may think I have a fixation with
[raw strings](2011/08/07/raw-strings-in-racket.html), but if you're
like most developers and have used different languages through the
years, you have probably experienced the feeling of wanting to port
your favourite features from language to language.

Most languages just won't let you do anything about it, but then,
happily, there is also a whole family of _programmable programming
languages_: [Lisps](http://www.paulgraham.com/chameleon.html) being
the most notable members among them, there are, for sure, other
languages in other sometimes forgotten paradigms.

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
language features can be put to work in a concatenative language.

## Factor as a shell

Factor has a very terse syntax. This can be good or bad, depending on
your application and the way your brain is wired up. Lately, I've
started using its
[visual repl](http://re-factor.blogspot.com.es/2010/09/visual-repl.html)
as an os shell on steroids (I'll eventually blog on the experience). I
think it makes sense since I spend a good share of my time on Windows
and I'm not crazy about the syntax of Powershell. A shell is clearly
one of the applications where less typing is just the right thing, and
the stack-based model sometimes feels like a natural upgrade of unix
piping (I'm thinking of the
[dataflow combinators](http://docs.factorcode.org/content/article-dataflow-combinators.html)
here).

## Raw strings

And, of course, being able to shamelessly use backslashes inside
strings is something you ask of a windows shell. I want to be able to
type something like:

{% highlight factor %}
r| \\Server\share| directory-files
{% endhighlight %}

Note the space after the first vertical bar. This is typical of
Factor, you'll see why in a moment.

As a Factor newbie, there are two things you can do. One is to RTFM,
which is extensive and well-written:

>   The Factor parser follows a simple recursive-descent design. The
>   parser reads successive tokens from the input; if the token
>   identifies a number or an ordinary word, it is added to an
>   accumulator vector. Otherwise if the token identifies a parsing
>   word, the parsing word is executed immediately.

The other one is to check the implementation of something close to
what's intended. This is extremely easy in Factor, since most of the
Factor libraries are implemented in Factor itself and the help system
lets you navigate through the definitions. The solution presented here
is inspired by the regex-introducing parsing word `R/`.

So it looks like we'll need to introduce a word that will hook up to a
function whose responsibility will be to push a string into the
accumulator. Something like:

{% highlight factor %}
SYNTAX: r| 124 parsing-raw ;
{% endhighlight %}

This means that the parser will immediately evaluate `124 parsing-raw`
after seeing the introductory `r|`. since this introductory word is
handled by the default lexer and parser, there needs to be a space
after it for Factor to process the end of word. This might seem
unnatural if you do not know Factor, but it is consistent with the
rest of the language.

124 is the ascii code for the vertical bar, which will act as a
terminator of the string to parse. Passing the terminator as a
parameter will make things easier if we decide to change the
separators someday.

## The lexer

For our purpose, we cannot work at the parser level: we don't deal
with words, numbers or already-constructed strings. If we want to
construct a Factor string in a different way, we'll have to call the
lexer directly. The lexer is stored in a [dynamic variable](http://docs.factorcode.org/content/article-namespaces.html) named `lexer`.

{% highlight factor %}
: parsing-raw ( accum end -- accum )
        lexer get take-until suffix! ;
{% endhighlight %}

`parsing raw` is reponsible for taking input until the `end` character
(124) is reached, and then suffixing the accumulator with the newly
parsed string.

The actual parsing is done in the `take-until` word:

{% highlight factor %}
: take-until ( end lexer -- string )
        [
                [ 1 + ] dip
                [ index-from ] 2keep
                [ swapd subseq ]
                [ 2drop 1 + ] 3bi
        ] change-lexer-column ;
{% endhighlight %}

The word `change-lexer-column` calls its quotation with the column and
the line text of the lexer at that moment. The first line, then, just
skips that blank we talked earlier following the introductory word
`r|`. The next two lines find the positon of the terminator
(`index-from`) and extract the string accordingly (`subseq`). At the
end of the quotation `change-lexer-column` finds the new lexer column
at the top of the stack, and just below it lies our return value: the
raw string.

Let's try it:

{% highlight factor %}
IN: scratchpad r| \\Server\share|

--- Data stack:
"\\\\Server\\share"
IN: scratchpad 
{% endhighlight %}

Done!

## Resources

- Slava's post on
  [writing DSLs on Factor](http://factor-language.blogspot.com.es/2009/09/survey-of-domain-specific-languages-in.html)
  gives a nice overview of Factor's self-modifying capabilities.
- The [docs](http://docs.factorcode.org/content/article-parsing-words.html), of course.

