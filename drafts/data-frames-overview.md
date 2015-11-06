---
title: The Dataframe Abstraction
date: 2000-01-01T00:00:00
tags: data, data-frame
---

# Introduction

In Software Engineering, there are rare occasions where an abstraction seems to
fit a problem so well that it becomes *the* way to tackle it. One of the most
successful abstractions for relational data processing is the *Dataframe*.
Originally used in `S`, made its way into [R](https://www.r-project.org/),
[pandas](http://pandas.pydata.org/), [Julia](http://julia-lang.org) 
 [clojure](http://incanter.org). I would
like to write a series of blog-posts describing how to wrangle data with these
different implementations, but before that it's worth to take a minute to think
about the abstraction itself. You can think of this article as a very informal,
pragmatic description of relational algebra for the working programmer.

# Operations

The dataframe is so pervasive because it is just a convenient implementation of
a [relation](https://en.wikipedia.org/wiki/Relational_algebra), as described by
[Codd](https://en.wikipedia.org/wiki/Edgar_F._Codd).

Most programmers will be more familiar with the _table abstraction_ in a
relational DB, which is also an implementation of the same principles. In fact,
most programmers will find this definition useful: the dataframe is an object
that allows the same kind of operations performed between SQL tables, but
_inside your application_, in a convenient way. These operations are, loosely:

- The *projection* of attributes. That meas that, from a dataframe, we can
  generate another one that only contains the attributes we are interested in.
  In SQL, that is what you do when specifying the columns after a `SELECT`.

- The *selection* of rows based on some logical conditions. That is what you do
  when specifying a `WHERE` clause SQL.

- *Merging* different dataframes based on some properties. These are the
  operations that people perform when using `JOIN`s in `SQL`. There are
  different kind of joins, which I will not discuss in detail now. The
  [wikipedia](https://en.wikipedia.org/wiki/Relational_algebra) will be enough
  for my explanations.

- *Aggregating* data can be further classified in sub-groups based on some
  logical criteria and some measures can be taken for each group. This is what
  you get through the `GROUP BY` and the aggregating functions (`COUNT`, `AVG`)
  in SQL.

So, if everyting is down there in SQL and there are so many quality OS
databases, why do we need those dataframe libraries at all? Well, here are some
reasons:

- For problem domains where you care about _analysis_ more than _curation_. That
  is the typical case for statistical packages such as `R`, where a statistician
  is concerned with describing or modelling the data more than actually making
  it available for querying or long-term cataloging.

- To combine data coming from different sources: a db, Excel files, CSV, XML,
  web scraping, etc...

- In a step where you do not yet know the structure of your data, so a DB schema
  cannot be properly described. Most dataframes allow for flexible typing, so
  these tools can be useful even as a tool to analyze the eventual schema of a
  relational DB.

- For one-shot analysis or attribute extractions, where the overhead of setting
  up a DB is not desireable.

- Dataframes are often implmented as some kind of library withing a language, so
  you get all the eventual goodies of the host. For example, in `R` you get all
  the analysis in the [CRAN](https://cran.r-project.org/). For Pandas, you get
  all the power of scientific python, etc...

