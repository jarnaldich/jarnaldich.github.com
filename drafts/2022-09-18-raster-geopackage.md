---
title: On Raster GeoPackages
date: 2022-09-18T00:00:00
tags: geopackage, raster, tiles
---

In a [previous article](/blog/2022/04/22/cloud-optimized-vector.html) I
discussed how the use of [image
pyramids](https://en.wikipedia.org/wiki/Pyramid_(image_processing)) is a
fundamental algorithm for optimizing the access of geospatial datasets. Just as
a quick recap, the dataset is splitted in several square tiles representing a
geographical section in a given projection, and then lesser resolution
_overviews_ or _pyramid levels_ are computed by some sort of
generlization/subsampling and also organized by tiles. Each tile in a coarser
level can be computed from tiles in the finer one, and represents a greater
geographical area at a coarser resolution. The following scheme might help
visualize the process:

![&nbsp;](/images/pyramid.jpeg "pyramid mage"){ width=400px .center }
<p><center><small>Source: OsGEO Wiki</small></center></p>

The optimization comes from the fact that a user/client can choose to read a
slice (subset of tiles) of the data at the resolution (pyramid level) needed for
an application, giving the opportunity to minimize data transfer. For example,
for visualization the data needed is limited by screen (window) size and
resolution, so it is never needed to fetch more pixels than the ones that fit in
the screen. At smaller scales the client can read from an overview and at higher
resolution only a slice of a narrower geographical area will need to be fetched.

With this algorithm, a tile can be uniquely indexed by its zoom level and its
column and row within the level matrix, eg `z/x/y`. These `z`, `x` and `y`
components are discrete and thus can be encoded by nonnegative integers. A
_Tiling Scheme_ is then needed to match this tile indices to an unambiguous tile
bounding box in a particular projection. This information can be described (for
the formats/applications that need to do so) with
the [OGC Two Dimensioal Tile Matrix Set
standard](http://docs.opengeospatial.org/is/17-083r2/17-083r2.html), which
(roughly) boils down to providing the following information:

- The underlying SRS.
- How many pyramid levels are needed (zoom_level index).
- For each level:
  - The scale denominator for the level (resolution of the level)
  - Top left coordinate of the first tile.
  - Size of each tile.
  - Total number of tiles in the level.
  
It is easy to see that with this information one can go back and forth between
`z/x/y` tile indices and ground coordinates in the given SRS.

Now an important point: these _Tiling Schemes_ can be locally defined by the
dataset or global. For example, a COG file can (implicitly) define a tiling
scheme based on the extent of the data it contains, while OpenMapTiles et al
define a tiling scheme [covering the whole
world](https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale). 
Some tools can be configured to work with different tiling schemes (even user
defined ones), so users can define their own dependeing on their project or
product.


Now, this algorithm can be embodied in different storage formats. For example, a
[tile server](https://mapproxy.org/) might choose to store the whole pyramid as
different files on disk (eg. `<zoom>/<x>/<y>.jpg`), and a GeoTiff software library will use
 the internal tiff directory to point to the offset of each tile. Or, in the
case of a GeoPackage, can be stored as a SQLite table holding the zoom_level, x,
y coordinates and a blob holding the image binary content.

# Parallel workflows

A powerful advantage of working with a global (or project-wise) tiling schemes
is that each tile gets a unique identifer regardless of the file/directory
holding the dataset. For example, if a tile server stores data from Europe in a
directory and from America in another one, merging them at the base level can be as
easy as copying one directory into the other... or almost: the only caveat is
when data contents are not aligned multiple of tile coordinates: that is, when
some tiles will be only partially full, and each dataset potencially fills a
different area within the tile. Merging those partially full tiles will
require extra work (typically alpha compositing of all the versions of a tile
with the same index in the different datasets).
This latter situation is actually very common at the overview level, as tiles
cover more and more area.

In the same vein, merging two raster geopackages corresponding to different
locations should be as easy as concatenating the contents of the table storing
the tile data.

These makes it easy to perform tasks over a dataset in parallel. One typical
ditributed processing scheme for geospatial workloads implies partitioning over
the total area to be processed. This can be achieved in a tiled dataset by
partitioning tile subsets among different workers and then be merged back again.

# GeoPackage as a tile repository

[GeoPackage](https://www.geopackage.org/) is a standard format for storing
geospatial information, including the tile matrix sets we have described. Best
of all: it is powered by a `SQLite` database, so a plain `SQLite` client (and
plain SQL code) can be used to explore the contents of such a file. This makes the
format surprisingly hackable. 

For example, taking advantage of SQLite's capability to attach different
databases with `ATTACH <file.sqlite> AS src`, we can merge two datasets by first
merging the non overlapping tiles:

```sql
INSERT INTO tiles(zoom_level, tile_column, tile_row, tile_data)
SELECT z.zoom_level, z.tile_column, z.tile_row, srctiles.tile_data
FROM (
    SELECT srctiles.zoom_level, srctiles.tile_column, srctiles.tile_row
    FROM src.tiles AS srctiles
    EXCEPT
    SELECT zoom_level, tile_column, tile_row FROM tiles
) AS z,
src.tiles srctiles
WHERE
    srctiles.zoom_level = z.zoom_level AND
    srctiles.tile_row = z.tile_row AND
    srctiles.tile_column = z.tile_column
```

And this query will return the tiles present in both datasets that might need to
undergo a process of alpha blending:

```sql
SELECT srctiles.zoom_level, srctiles.tile_column, srctiles.tile_row,
    srctiles.tile_data, dsttile.tile_data
FROM src.tiles AS srctiles, tiles AS dsttile
WHERE srctiles.zoom_level = dsttile.zoom_level AND
      srctiles.tile_row = dsttile.tile_row AND
      srctiles.tile_column = dsttile.tile_column
```

Of course, the metadata table will need to be updated in order to adjust the
bounding box of the merged dataset.

# Using GDAL to generate raster GeoPackages

From version 3.2 onwards, [gdal](https://gdal.org) can generate raster
GeoPackages with custom tileset descriptions through its [Raster
GeoPackage](https://gdal.org/drivers/raster/gpkg.html) driver. Using a custom
tile scheme can be tricky, so here are a few hints for RGB imagery data. I am
assuming a non-trivial source format: 16b 4-Channel (RGB, Near
Infrared) imagery with a special nodata value.

First, raster GPKG work with 3 band 8 bit images, so bands have to be selected
and scaled. Luckyly we can do so without generating bulky intermediate files by
taking advantage of the `VRT` driver.

```
gdal_translate -of VRT -scale 1 65535 1 255 -ot Byte -b 1 -b 2 -b 3 <input.tif> <output8b3Band.vrt> 
```

To add an alpha layer we will need to use an option of `gdalwarp`, not existing
in `gdal_translate`. Furthermore, `gdal` is quite picky with the limit alignment
of the chosen tiling scheme, so the same execution can be used to ensure that
the dataset is properly aligned. Otherwise we could get an error down the road
stating that no matching tile matrix level can be found. Again this will be fast
and take no space if we stick to the `VRT` format.

```
gdalwarp -tap -tr 0.1 0.1 -srcnodata 0 -dstalpha -of VRT <output8b3Band.vrt> <warped_alpha.vrt>
```

Finally, the geopackage can be generated this is the step that will materialize
the previous virtual rasters:

```
gdal_translate -of GPKG -co RASTER_TABLE=tiles -co TILING_SCHEME=<tiling_scheme.json> <output8b3Band.vrt> <output.gpkg>
```

After that, we can generate overviews for the dataset:

```
gdaladdo -r average <output.gpkg>
```
