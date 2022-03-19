
Extract Transform Load (ETL) is a broad term for processes that read a subset
of data in one format, perform a more or less involved transformation and store
data (probably) in another format. Those processes can of course be chained
together to form larger data pipelines. As in many such general terms, this can
mean very different things in terms of software architecture and
implementations. For example, depending on the scale of the data the solution
may range from  unix shell pipeline to a full-blown [Apache
nifi](https://nifi.apache.org/) solution.

One common theme is data impedance mismatch between formats. Take for example
JSON and XML. They are surely different, but for any
particular application you can find a way to move data from one to the other.
They even have their own [traversal
systems](https://chrispenner.ca/posts/traversal-systems) (`jq`'s syntax and `XPath`).

In this blog post we will explore an elegant, if not efficient, way to perform
such transformations using Haskell.

## The problem

Haskell is a curiously effective fit for this kind of problems due to the
unlikely combination of three seemingly unrelated traits: its parsing libraries
(driven by a community interested in programming languages theory), *optics*
(also driven by PLT and a gruesome syntax for record accessors, up to GHC
9.2.1 `RecordDotSyntax`), and the convience for writing scripts with the `stack`
tool (driven by the olden unreliability of `cabal` builds). Let's break down how
these work together.

### Parsing libraries

Let's face it: programming language junkies were a good share of Haskell´s
comunity (at least until the advent of the Crypto ~~scam~~). But, more important
than the availability of parsing libraries itself, it's the [parse, don't
validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
approach in this libraries that works here: most of them have the ability to decode
(deserialize,parse) its input into a well typed structured value in memory.

### Optics

Optics (lenses, prisms, traversals) are way to abstract getters and setters in a
composable way. Their surface syntax reads like "pinpointing" or
"bookmarking" into a deeply nested data structure (think `XPath`), which make it
nice for visually keeping track of what is being read or altered.

### Scripting

A lot of the data wrangling programs are one-shot scripts, where you care about
the result more than about the software itself. Having to create a new app each
time can be tiresome, so using scripting and knowing you can rely on a set of
curated libraries to get the job done is really nice. Starting with a script
that can be turned at any time into a full blown app that works on all the major
platforms is also nice.


http://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?format=xml
https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json
https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
