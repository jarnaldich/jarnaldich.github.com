---
title: ETL The Haskell Way
date: 2022-03-27T00:00:00
tags: haskell, data
---

Extract Transform Load (ETL) is a broad term for processes that read a subset
of data in one format, perform a more or less involved transformation and
then store it in a (maybe) different format. Those processes can of course be
linked together to form larger data pipelines. As in many such general terms,
this can mean very different things in terms of software architecture and
implementations. For example, depending on the scale of the data the solution
may range from unix shell pipeline to a full-blown [Apache
nifi](https://nifi.apache.org/) solution.

One common theme is data impedance mismatch between formats. Take for example
JSON and XML. They are surely different, but for any
particular application you can find a way to move data from one to the other.
They even have their own [traversal
systems](https://chrispenner.ca/posts/traversal-systems)
([jq](https://stedolan.github.io/jq/)'s syntax and
[XPath](https://developer.mozilla.org/en-US/docs/Web/XPath)).

The most widely used solution for small to medium data is to write small ad-hoc
scripts. One can somewhat abstract over these formats by [abusing
jq](https://blog.lazy-evaluation.net/posts/linux/jq-xq-yq.html).

In this blog post we will explore more elegant way to perform such
transformations using Haskell. The purpose of this post is just to pique your
curiosity with what's possible in this area with Haskell. It is definitely *not*
intended as a tutorial on optics, which are not for Haskell beginners, anyways... 

## The Problem

We will be enriching a [geojson](https://datatracker.ietf.org/doc/html/rfc7946)
dataset containing [countries](static/countries.geo.json) at a world scale taken
from natural earth and enriching it with [population data in
xml](static/population.xml) as provided by the world bank API so that it can be
used, for example, to produce a [choropleth]() ~~map~~[^map] visualization.

![&nbsp;](/images/worldpop.png "this is not a map"){ .center }

Haskell is a curiously effective fit for this kind of problems due to the
unlikely combination of three seemingly unrelated traits: its parsing libraries
(driven by a community interested in programming languages theory), *optics*
(also driven by PLT and a gruesome syntax for record accessors, at least up to the recent
addition of `RecordDotSyntax`), and the convience for writing scripts with the `stack`
tool (driven by the olden unreliability of `cabal` builds). 

It is the fact that Haskell is so _abstract_, that makes it easy to combine libraries
never intended to work together in the first place. Haskell libraries tend to
define its interfaces in terms of very general terms (eg. structures that can be
mapped into, structures that can be "summarized", etc..).

Let's break down how these work together.

### Parsing Libraries

Haskell comes from a long tradition of programming language theory applications,
and it shines for building parsers, so there is no shortage of libraries for
reading the most common formats. But, more important
than the availability of parsing libraries itself, it's the [parse, don't
validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
approach in this libraries that works here: most of them have the ability to decode
(deserialize,parse) its input into a well typed structured value in memory
(think Abstract Syntax Tree).

So a typical workflow would be to read the data from disk into a more or less
abstract representation in memory involving nested data structures, then
transform it into another representation in memory (maybe generated from a
template) through the use of optics and then serialize it back to disk:

![&nbsp;](/images/haskell_lens_workflow.png "Haskell lens workflow"){ .center }

### Optics

Optics (lenses, prisms, traversals) are way to abstract getters and setters in a
composable way. Their surface syntax reads like "pinpointing" or
"bookmarking" into a deeply nested data structure (think `XPath`), which make it
nice for visually keeping track of what is being read or altered.

The learning curve is wild, and their error messages convoluted, but the fact
that in Haskell we can abstract accessors away from any particular data
structure, and that there are well-defined functions to combine them can reduce
the size of your data transformation toolbox. And lighter toolboxes are easier
to carry around with you.

### Scripting

A lot of the data wrangling programs are one-shot scripts, where you care about
the result more than about the software itself. Having to create a new app each
time can be tiresome, so using scripting and knowing you can rely on a set of
curated libraries to get the job done is really nice. Starting with a script
that can be turned at any time into a full blown app that works on all the major
platforms is also nice.

## The Solution

The steps follow the typical workflow quite closely, in our case:

1. Parse the `.xml` file into a data structure (a document) in memory.
2. Build a map from country codes to population.
3. Read the geojson file with country info and get the array of features.
4. For each feature, create a new key with the population.

This overall structure can be traced in our main function:

```haskell
main = do
  xml <- XML.readFile XML.def "population.xml" -- Parse the XML file into a memory document
  let pop2020Map = Map.fromList $ runReader records xml -- Build a map Country -> Population
  jsonBytes <- LB8.readFile "countries.geo.json" -- Parse the countries geojson into memory
  let Just json = Json.decode jsonBytes :: Maybe Json.Value
  let featureList = runReader (features pop2020Map) json :: [ Json.Value ] -- Get features with new population key
  let newJson = json & key "features"  .~ (Json.Array $ V.fromList featureList) -- Update the original Json
  LB8.writeFile "countriesWithPopulation.geo.json" $ Json.encode newJson -- Write back to disk

```

The form of the input data is not especially well suited for this app. The world
population xml is basically a table in disguise (remember the data impedance
problem?). It is basically a list of:

```xml
    <record>
      <field name="Country or Area" key="ABW">Aruba</field>
      <field name="Item" key="SP.POP.TOTL">Population, total</field>
      <field name="Year">1960</field>
      <field name="Value">54208</field>
    </record>
```

That means the function that reads it has to associate information from two
siblings in the XML tree, but that is easy using the `magnify` function inside a
`Reader` monad:

```haskell
records :: Reader XML.Document [(T.Text, Scientific)]
records =
  let
    -- Lens to access an attribute from record to field. Intended to be composed.
    field name = nodes . folded . _Element . named "field" . attributeIs "name" name
  in do
    -- Zoom and iterate all records
    magnify (root . named "Root" ./ named "data" ./ named "record") $ do
      record <- ask
      let name = record ^? (field "Country or Area" . attr "key")
      let year = record ^? (field "Year" . text)
      let val  = record ^? (field "Value" . text)
      -- Returning a monoid instance (list) combines results.
      return $ case (name, year, val) of
        (Just key, Just "2020", Just val) -> [ (key, read $ T.unpack val) ]
        _ -> []
```

Note how lenses look almost like `XPath` expressions. The `features` function
just takes the original features and appends a new key:

```haskell
features :: Map.Map T.Text Scientific -> Reader Json.Value [ Json.Value ]
features popMap = do
  magnify (key "features" . values) $ do
    feature <- ask
    let Just id = feature ^? (key "id" . _String) -- Gross, but effective
    return $ case (Map.lookup id popMap) of
      Just pop -> [ feature & key "properties" . _Object . at "pop2020" ?~  Json.Number pop ]
      _ -> [ feature ]
```

That is really all it takes to perform the transformation. Please take a look at the
full listing in [this
gist](https://gist.github.com/7cb4fd07bc8689f5c3bccb58b2e239ae#file-etl-hs).
Even with the imports, it cannot get much shorter or expressive than
this fifty something lines...

## Revenge of the Nerds

So Haskell turns out to be the most practical, straightforward solution I
found for this kind of problems. Who knew?

I would absolutely not recommend learning Haskell just to
solve this kind of problems (although I would absolutely recommend learning it
for many other reasons). This is one of the occasions in which learning
something just for the sake of it pays off in unexpected ways. 

[^map]: No lengend! No arrow pointing north! Questionable projection! This is
    not a post on map making, just an image to ease the reader's eye after too
    much text for the internet...
