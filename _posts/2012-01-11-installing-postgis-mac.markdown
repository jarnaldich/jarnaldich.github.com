---
layout: post
title: Installation Report - Installing PostGIS on Mac Os X
tags: postgres postgis mac
---

In this post I will basically document the process of installing
_postgis_ in my own machine for future reference. This will interrupt
the scheme implementation series I had been posting lately, but my
job's getting in the way and I don't want to forget about this. I just
post it in case anyone else finds it useful.

_Postgis_ is a geospatial layer on top of the open-source database
_postgres_. But if you didn't know that, then you probably shouldn't
be reading this anyway... At the moment of posting, the latest version
for _postgresql_ is 9.1.2, and for _postgis_ is 1.5.3.

Get Homebrew
------------

[HomeBrew](http://mxcl.github.com/homebrew/) is _the_ package manager
for Mac Os X. Installation is straighforward following the
instructions on their webpage, and it opens the door a whole lot of
open-source applications.

Install postgresql
------------------

1. `brew install postgresql`. 
2. Create the data directory somewhere in your tree. `/usr/local/var/posgres` or
`/usr/local/var/pgdata` are popular choices. I'll stick to `pgdata`.
3. Initialize the db: `initdb /usr/local/var/pgdata/`. Pay attention
to the messages regarding the _locale_. 
4. Create the `adminpack` extension for use with _pgAdmin_. Just open
the `psql` shell on the postgres db (`psql -d postgres`) and type
`CREATE EXTENSION adminpack;`.  

Note that the default user will be the one that executed `brew`. This
is ok for a development machine, but you should set up your own user
if you are planning to use the server in production.

You should now be able to start postgres with:


    postgres -D /usr/local/var/pgdata

or

    pg_ctl -D /usr/local/var/pgdata -l logfile start

Install pgAdmin
---------------

Download the latest version from
[postgres ftp](http://www.postgresql.org/ftp/pgadmin3/release/). It is
a regular _dmg_ package, so just drag it to Applications.

You can now create a test db:

    psql -d postgis_test -f
    /usr/local/Cellar/postgis/1.5.3/share/postgis/postgis.sql
    
If you need reference system information, you will also need to feed
in the `spatial_ref_sys.sql` file:

    psql -d postgis_test -f
    /usr/local/Cellar/postgis/1.5.3/share/postgis/spatial_ref_sys.sql
  
Now you should be able to load some data with `shp2pgsql` and dump it
with `pgsql2shp`.

Resources
---------

- The postgis
  [manual](http://postgis.refractions.net/documentation/manual-1.5/ch02.html).
- This is
  [a nice tutorial](http://www.bostongis.com/?content_name=postgis_tut01)
  for windows users.
