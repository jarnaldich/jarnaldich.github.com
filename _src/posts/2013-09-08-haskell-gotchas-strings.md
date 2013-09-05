    Title: Haskell Gotchas: Strings
    Date: 2013-09-09T00:00:00
    Tags: DRAFT, haskell, gotchas, strings

I've been playing with [Haskell](http://www.haskell.org) on and off
for some time now. Let's face it: it's not an easy language to
learn. I would agree with most people that part of the difficulty
comes from its unique features (laziness, purity, etc...), and the
abstractions that derive from them when facing real-world problems
(Monads, Iteratees, etc...), but my point is that there are other
sources of confusion that keep misleading beginners like myself and
that have nothing to do with all that.

In fact, I can see a pattern in those
[gotchas](http://catb.org/~esr/jargon/html/G/gotcha.html): Haskell
often provides several tools for a particular task. It is (too)
often the case that the one that's most at hand, or that seems to be
favored in the docs, is just not what a newbie would probably
want/expect. I think this fact has an impact on the perception of
first-time users, if they do not perservere enough to seek for
alternatives.

I am planning to write a series of posts on these gotchas, if only to
prevent myself from falling into the same traps the next time I decide
to take on Haskell. I hope they're useful to other learners. Please
mind that I have no valorative intetion whatsoever (no flames: it is
not a Haskell [WTF]?

