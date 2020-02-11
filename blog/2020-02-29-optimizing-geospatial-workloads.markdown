---
title: Optimizing Geospatial Workloads
date: 2020-02-29T00:00:00
tags: geospatial, minizinc, optimization
---

Large area geospatial processing often involves splitting into smaller
working tiles to be processed or downloaded independently. As an example,
25cm resolution orthophoto production in Catalonia is divided into 4275
rectangular tiles, as seen in the following image.

![](/images/tiles5k.png "Orthophoto Tiling")

Whenever a process can be applied to those tiles independently (ie, not
depending on their neighborhood), parallel processing is an easy way to increase
the throughput. In such environments, the total workload has to be distributed
among a fixed, often limited, number of processing units (be they cores or
computers). If the scheduling mechanism requires a predefined batch to be
assigned to each core (or if there is no scheduling mechanism at all), and when
the processing units are of similar processing power, then the maximum speedup
is attained when all batches have an equal amount of tiles.

Furthermore, since the result often has to be mosaicked in order to inspect it,
or to aggregate it into a larger final product, it is desireable for the
different batches to keep a spatial continuity, ideally axis parallel
rectangles, since that is the basic form of georeference for geospatial imagery
once projected.

## The problem

This is a discrete optimization problem, which can be solved using the regular
machinery. Since I have been dusting off my [MiniZinc](https://www.minizinc.org)
abilities through Coursera’s discrete optimization series, I decided to give it
a go.

### Tile scheme representation

For convenience, the list of valid orthophotos can be read as a list from an external data file `.dzn`

``` 
int: northos;
enum e_ortho = { ocol, orow };
array[1..northos, e_ortho] of int: Orthos;
```

We can construct this array from a listing of the valid rows and columns for the
tiling scheme. 

```
northos = 4275;
Orthos = [| 253, 055
          | 254, 055
          | 253, 056
          | 254, 056
          | 255, 055
          | 255, 056
          | 256, 056
          | 257, 056
          | 252, 059
          …
          |];
```

From the above data, the grid can be represented as a 2d array within the bounds
of a minimum and maximum columns.


``` ampl
int: mincol = min([ Orthos[i, ocol] | i in 1..northos ]);
int: maxcol = max([ Orthos[i, ocol] | i in 1..northos ]);
int: minrow = min([ Orthos[i, orow] | i in 1..northos ]);
int: maxrow = max([ Orthos[i, orow] | i in 1..northos ]);

array[minrow..maxrow, mincol..maxcol] of int: Grid =
  array2d(minrow..maxrow, mincol..maxcol,
     [ exists(i in 1..northos)(Orthos[i, orow] == r /\ Orthos[i, ocol] == c)
       | r in minrow..maxrow, c in mincol..maxcol ]);
```

### Box representation

``` 

```

## Conclusions

Even when I know the basic theory behind mixed integer and fp solvers (even
implemented a simplex-based solver as a practical exercise for another course),
I keep having the feeling there is some form of magic at work here.

## Repa

A library I found particularly compelling was
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
These intermediate lists waste time and space doing useless temporary
allocation and garbage collection.

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

![Voronoi diagram](/images/voronoi.png "Voronoi diagram")

It is a 512x512 images with 150 random centers. The colored polygons
represent the areas which are closest to a particular center. The most
popular algorithm for computing a Voronoi diagram in 2 dimensions
seems to be
[Fortune's algorithm](http://en.wikipedia.org/wiki/Fortune's_algorithm).
There are also nice open-source implementations out there: for real
work, I'd recommend the excellent
[qhull library](http://www.qhull.org/html/qvoronoi.htm).

Since I was just interested in testing parallelism, I decided to
implement it the
[Rosetta Code](http://rosettacode.org/wiki/Voronoi_diagram) way, which
boils down to just applying the definition: take an image and a random
set of in-range pixel coordinates (the centers). For each pixel, color it
according to the center that lies closest (in our case, closest
according to the euclidean metric). This algorithm is embarassingly
naive, but also
[embarassingly parallel](http://en.wikipedia.org/wiki/Embarrassingly_parallel),
since each pixel can be computed independently.

## The Source

The code is pretty straightforward. You can find the whole source
[here](https://raw.github.com/jarnaldich/jarnaldich.github.com/master/_src/posts/voronoi.hs),
or on the [Rosetta Code](http://rosettacode.org/wiki/Voronoi_diagram)
page.

I'll comment on the most important parts.

First, we need a function for the metric to minimize. To make it
faster, we will not take the square root. We will also use strict
annotations and 32 bit unsigned integers (`Word32`), instead of
Haskell's unbounded `Int`s. Finally, we will tell GHC to inline it,
since `Repa` recommends making extensive use of inlining (as always,
when in doubt, profile).

```haskell
{-# INLINE sqDistance #-}    
sqDistance :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
sqDistance !x1 !y1 !x2 !y2 = ((x1-x2)^2) + ((y1-y2)^2)
```

Getting a random array with the centers is easy and shows the way
to generate unboxed arrays from a list in `repa`:

```haskell
centers :: Int -> Int -> Array U DIM2 Word32
centers nCenters nCells =
      fromListUnboxed (Z :. nCenters :. 2)
    $ take (2*nCenters)
    $ randomRs (0, fromIntegral (nCells - 1)) (mkStdGen 1)
```

Note the type signature: `Array U DIM2 Word32` means unboxed array
(the `U`), indexed by 2 integers (the `DIM2`) and storing `Word32`
values. The indexing is a bit tricky, since is mostly done with the
`:.` operator, and the notation is somewhat overloaded to denote
indexes and shapes, so `arr Repa.! (Z:. i :. j)` means the element in
the ith row and jth column of the array arr, but `fromListUnboxed (Z
:. nCenters :. 2)` means we are creating an array of `nCenters` rows
and 2 columns.

Now there come two helper functions. The first one takes a 2 column
matrix and a two parameter function and returns the array resulting of
applying the function to each row. 

```haskell
applyReduce2 arr f = 
    traverse arr (\(i :. j) -> i) $ \lookup (Z:.i) ->
        f (lookup (Z:.i:.0)) (lookup (Z:.i:.1))
```

In order to do so, this function makes use of a very powerful `repa`
combinator, which takes a function on the indices (in this case,
reduces one dimension) and a two parameter function. The first
parameter is itself a lookup function on the input array, while the
second one is the index of the output array whose value we are trying
to compute. Please refer to the
[tutorial](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial)
if this is not clear enough.

Apart from `traverse`, there are more familiar combinators, like
`foldS`, which is just like a fold for arrays. We make use of it to
compute the minimum of a function over an array. The final `S` stands
for "sequential". Some repa combinators come in two flavors:
sequential ones or parallel ones (would be `foldP`). For this
algorithm we will parallelize only the pixel loop, so we are using the
sequential version for the minimization loop. Here's the minimization
function, which basically decorates the array with an index before
folding over it:

```haskell
minimize1D arr = foldS f h t
  where
    indexer src idx@(Z :. i) = (src idx, (fromIntegral i))
    indexed arr = traverse arr id indexer
    (Z :. n) = extent arr
    iarr = indexed arr
    h = iarr ! (Z :. 0)
    t = extract (Z :. 1) (Z :. (n-1)) iarr
    f min@(!valMin, !iMin ) x@(!val, !i) | val < valMin = x
                                         | otherwise = min
```

With these helpers, writing a parallel voronoi is easy. We will make
use of `fromFunction` to create a *delayed* array, which we can later
force to compute.

```haskell
voronoi :: Int -> Int -> Array D DIM2 Word32
voronoi nCenters nCells =
    let
      cellReducer = applyReduce2 (centers nCenters nCells)
      nearestCenterIndex = snd . (Repa.! Z) . minimize1D
      {-# INLINE builder #-}
      builder (Z:.i:.j) = nearestCenterIndex
                        $ cellReducer $ on sqDistance fromIntegral i j
    in        
      Repa.fromFunction (Z :. nCells :. nCells :: DIM2) builder
```

The `voronoi` function creates a matrix of integer indices, referring
to the center which is closest. If we want to write that as an RGB
image, we will also need a function to create a random color table and
another one to colorize the voronoi array:

```haskell
genColorTable :: Int -> Array U DIM1 (Word8, Word8, Word8)
genColorTable n = fromListUnboxed (Z :. n) $ zip3 l1 l2 l3
    where
      randoms = randomRs (0,255) (mkStdGen 1)
      (l1, rest1) = splitAt n randoms
      (l2, rest2) = splitAt n rest1
      l3 = take n rest2

colorize ctable = Repa.map $ \x -> ctable Repa.! (Z:. fromIntegral x)
```

As we can see, the colorized table will be a two dimensional array of
3-element tuples: one for the red, green, and blue components. This is
the format expected by `writeImageToBMP` in the `Repa.IO.BMP` package.

with all the above, the main function will look like:

```haskell
main = do
  let nsites = 150
  let ctable = genColorTable nsites 
  voro <- computeP $ colorize ctable $ voronoi nsites 512
  writeImageToBMP "out.bmp" voro
```

There are some cool things going on under the hood. First, note that
we just plugged the `colorize` and `voronoi` parts. In spite of this,
there will be no intermediate arrays: both calculations will be fused
into a single operation.

The second thing is the use of a parallel combinator `computeP`, which
will transform a delayed array into an unboxed one *in parallel*
(given the appropiate compilation options and runtime parameters).
Note that parallel computations in repa must run in some monad, to
ensure they are performed in the appropiate order. It can be any monad
(in this case, it's `main`'s `IO`).

Now, if we compile with

```
ghc -O2 -fllvm -fforce-recomp -threaded --make voronoi.hs  -o voronoi
```

My machine is a somewhat oldish 2GHz Intel Core 2 Duo with 4GB (1067Mhz
DDR3) of RAM. We can try to run it on one core:

```
$ time ./voronoi

real	0m3.015s
user	0m2.946s
sys	0m0.069s
```

or on two cores:

```
$time ./voronoi +RTS -N2

real	0m1.644s
user	0m3.101s
sys	0m0.068s
```

Note that the speedup is pretty good. We can also see detailed
statistics by the runtime system:

```
$ ./voronoi +RTS -N2 -s
   8,750,790,680 bytes allocated in the heap
       5,940,344 bytes copied during GC
         852,168 bytes maximum residency (2 sample(s))
          49,752 bytes maximum slop
               5 MB total memory in use (2 MB lost due to fragmentation)

                                  Tot time (elapsed)  Avg pause  Max pause
Gen  0      8640 colls,  8639 par    0.15s    0.11s     0.0000s    0.0033s
Gen  1         2 colls,     2 par    0.00s    0.00s     0.0003s    0.0005s

  Parallel GC work balance: 1.78 (690765 / 388167, ideal 2)

                        MUT time (elapsed)       GC time  (elapsed)
  Task  0 (worker) :    3.00s    (  1.57s)       0.18s    (  0.13s)
  Task  1 (worker) :    3.18s    (  1.69s)       0.00s    (  0.00s)
  Task  2 (bound)  :    3.18s    (  1.69s)       0.00s    (  0.00s)
  Task  3 (worker) :    2.97s    (  1.54s)       0.21s    (  0.15s)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    3.03s  (  1.59s elapsed)
  GC      time    0.15s  (  0.11s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    3.18s  (  1.69s elapsed)

  Alloc rate    2,884,832,617 bytes per MUT second

  Productivity  95.3% of total user, 179.1% of total elapsed

gc_alloc_block_sync: 3416
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 3
```

95% user productivity looks good to me. By comparison, and unoptimized
single-core `C` version out of
[Rosetta Code](http://rosettacode.org/wiki/Voronoi_diagram) takes
somewhat less than 2 seconds, while the optimized one takes around
half a second.

Please, take the times above with a grain of salt: I am sure a
seasoned haskeller would squeeze more speed out of my version, and
probably the `C` and the Haskell should'nt be compared in the first
place (the output image format is different).

## Conclusions

As we have seen, writing parallel array operations with a decent
performance is easy with `Repa`. While I am doubtful that it can reach
the speed of `C` without making the code just too ugly, IMHO the
balance between speed, ease of development and compositional style
makes `Repa` a worthwhile tool in your bag.

## References

Here are some cool links if you want to play around with Voronoi
diagrams:

- [Online demo](http://bl.ocks.org/mbostock/4060366)
- [Another one](http://www.raymondhill.net/voronoi/rhill-voronoi.html)
- [A spherical one](http://www.senchalabs.org/philogl/PhiloGL/examples/voronoi/)
- [The vorowiki](http://voronoi.com/wiki/index.php?title=Main_Page)
