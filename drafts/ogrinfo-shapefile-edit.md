---
title: Editing Shapefiles data with OgrInfo
date: 2021-09-15T00:00:00
tags: ogrinfo, gdal, ogr, osgeo, shapefile
---

Let's face it: shapefiles are a ridiculously popular format in the geospatial
industry. Even with all its shortcomings and better alternatives, there is
still an unreasonable amount of software using them, and an unreasonable amount
of users choosing them as their first option.

**picture of ADAMS family chainsaw**

As an example, TileIndices for MapServer need to be shapefiles. 

One of the inconvieniences of the shapefile format is their use of the old DBF
(dbase) format for storing feature data, which makes it difficult to find tools
to work with. For example, if you want to update a property in an automated
way, the most widely used solution is to fire up a GIS and rewrite the file or
write yourself a script leveraging ogr... Contrast this with what you would do
with a geojson file (probably just use a text editor) or a
geopackage/spatialite (where you could use a SQL script).

Well, most people do not know there is an option to use an SQL script /
one-liner to edit shapefiles too! Mainly because it is hidden in a tool that is
normally used to get information, not editing: good old `ogrinfo`.

It works with SQL one-liners

```
ogrinfo <layer>.shp -dialect SQLite -sql "UPDATE <layer> SET field=..."
```

As well as with scripts with the `\@` syntax prefix.

```
ogrinfo <layer>.shp -dialect SQLite -sql \@<script.sql>
```


