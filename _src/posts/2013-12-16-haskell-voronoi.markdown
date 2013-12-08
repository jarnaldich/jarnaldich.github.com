    Title: Parallelizing Voronoi in Haskell with repa.
    Date: 2013-12-16T00:00:00
    Tags: haskell, gotchas, strings, DRAFT

I recently bought a copy of *Parallel and Concurrent Programming in
Haskell*, by Simon Marlow, also available online
[here](http://chimera.labs.oreilly.com/books/1230000000929). It's a
very good overview for anyone who (like me) has ever been confused by
the wealth of different libraries and primitives for doing concurrency
& parallelism in Haskell.

So I thought I would put what I learned to work with an example of my
own.
<!-- more -->

## Repa

A library I found particularly compelling was the
[repa](http://hackage.haskell.org/package/repa). Quoting from the
[tutorial](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial),

> Repa is a Haskell library for high performance, regular,
> multi-dimensional parallel arrays. All numeric data is stored unboxed
> and functions written with the Repa combinators are automatically
> parallel...

Let's describe what makes `repa` fast step by step. Note that `repa`
heavily relies on the optimizations performed by the
[GHC](http://www.haskell.org/ghc/), so whenever I say Haskell in this
post, please think of the `GHC` stack.

### Unboxed types

Like in many other high-level languages, the default types in GHC are
*boxed*, meaning that they are represented by a pointer to a object in
the heap, rather than a primitive type itself. 


You can read more about unboxed types [in the manual](http://www.haskell.org/ghc/docs/7.0.1/html/users_guide/primitives.html).


## The Source

```haskell



```

## References
