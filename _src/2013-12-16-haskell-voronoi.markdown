    Title: Parallel Voronoi in Haskell
    Date: 2013-12-16T00:00:00
    Tags: haskell, voronoi, repa, parallel, DRAFT

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
the heap, rather than a primitive type itself. The use of boxed types
adds one level of indirection and thus has an impact on performance
because of the extra allocation and the loss of locality.

You can read more about unboxed types [in the manual](http://www.haskell.org/ghc/docs/7.0.1/html/users_guide/primitives.html).

### Stream fusion

Consider a function like this:

```haskell
squareAddTwo :: [Int] -> Int
squareAddTwo = sum . map (+2) . map (*3) 
```

It is supposed to multiply each element in an integer list by three,
add two, and then sum up all the numbers in the list. A naive
implementation of the above would use 3 lists: the input list and two
intermediate lists for storing the result of the two `map` operations.
These intermediate lists are costly in useless temporary allocation
and garbage collection.

Now, with stream fusion, equational laws are
applied to get rid of these intermediate structures in a process
called deforestation. The above could be translated into something
like:

```haskell
myFoldingSquareAddTwo = foldl' (\x y -> x + (y*3 + 2)) 0
```

Note that recent versions of GHC have deforestation built-in for
regular lists, so you can take advantage of fusion provided you stick
to the old suspects: `map`, `fold`, etc... If you implement your own
recursive functions, then GHC will *not* be able to fuse. Here is a
snippet that you can play with. I encourage you to try what is the
largest value of `n` for which this program correctly terminates:

```haskell
module Main where
import System.Environment
import Data.List (foldl')
    
myMap f [] = []
myMap f (h:t) = f h : myMap f t

mySum [] = 0
mySum (h:t) = h + mySum t

mySquareAddTwo = mySum . myMap  (+2) . myMap (*2) 

squareAddTwo :: [Int] -> Int
squareAddTwo = sum . map (+2) . map (*3) 

myFoldingSquareAddTwo :: [Int] -> Int
myFoldingSquareAddTwo = foldl' (\x y -> x + (y*3 + 2)) 0

main = do
  [n] <- getArgs
  print $ squareAddTwo [1..read n :: Int]
  print $ myFoldingSquareAddTwo [1..read n :: Int]        
  print $ mySquareAddTwo [1..read n :: Int]  
```

### Automatic parallelism

Repa provides a set of combinators for creating and manipulating
arrays. The operations needed to build an array are described
declaratively in a first step (creating a so-called *delayed* array),
and then the array is later materialized (which will give an *unboxed*
array).

This double process allows for `repa` not only to fuse away the
intermediate structures, but also to perform the required data
dependency analysis prior to parallelizing the computation. 

Hopefully, the Voronoi example will help you understand this process.

## Voronoi

Quoting from the [wikipedia](http://en.wikipedia.org/wiki/Voronoi_diagram):

> In mathematics, a Voronoi diagram is a way of dividing space into a
> number of regions. A set of points (called seeds, sites, or
> generators) is specified beforehand and for each seed there will be a
> corresponding region consisting of all points closer to that seed than
> to any other. The regions are called Voronoi cells

So we are trying to get a pretty picture like this one:

![Voronoi diagram](/img/voronoi.png "Voronoi diagram")

It is a 512x512 images with 150 random centers. The colored polygons
represent the areas which are closer to a particular center. The most
popular algorithm for computing a Voronoi diagram in 2 dimensions
seems to be
[Fortune's algorithm](http://en.wikipedia.org/wiki/Fortune's_algorithm).
For real work, I'd recommend the excellent
[qhull library](http://www.qhull.org/html/qvoronoi.htm).

Since I was just interested in testing parallelism, I decided to
implement it the
[Rosetta Code](http://rosettacode.org/wiki/Voronoi_diagram) way, which
is just applying the definition: take an image and a random set of
points (the center). For each pixel, colour it according to the center
that lies closest (in our case, closest according to the euclidean
metric). This algorithm is embarassingly bad, but also
[embarassingly parallel](http://en.wikipedia.org/wiki/Embarrassingly_parallel),
since each pixel can be computed independently.

## The Source

The code is pretty straight forward. You can find the whole source
[here](https://raw.github.com/jarnaldich/jarnaldich.github.com/master/_src/posts/voronoi.hs),
or on the [Rosetta Code](http://rosettacode.org/wiki/Voronoi_diagram)
page.

I'll comment on the most important parts.

First, we need a function with the metric to maximize. To make it
faster, I will not take square roots, I use strict annotations and a
32 bit unsigned integer (`Word32`). I also inline it. Repa recommends
making extensive use of inlining.

```haskell
{-# INLINE sqDistance #-}    
sqDistance :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
sqDistance !x1 !y1 !x2 !y2 = ((x1-x2)^2) + ((y1-y2)^2)
```

Getting the a random array with the centers is easy and shows the way
to generate unboxed arrays from a list in `repa`:

```haskell
centers :: Int -> Int -> Array U DIM2 Word32
centers nCenters nCells =
    fromListUnboxed (Z :. nCenters :. 2) $ take (2*nCenters) $ randomRs (0, fromIntegral nCells) (mkStdGen 1)
```

Note the type signature: `Array U DIM2 Word32` means unboxed array (the
`U`), indexed by 2 integers (the `DIM2`) and storing Word32 values.
The indexing is a bit tricky, since is mostly done with the `:.`
operator, and the notation is somewhat overloaded to denote indexs and
shapes, so `arr Repa.! (Z:. i :. j)` means the element in the ith row
and jth column of the array arr, but `fromListUnboxed (Z :. nCenters
:. 2)` means we are creating an array of `nCenters` rows and 2 columns.

## References

asdf
