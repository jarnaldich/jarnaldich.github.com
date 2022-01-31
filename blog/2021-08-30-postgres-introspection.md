---
title: Introspection in PostgreSQL
date: 2021-08-30T00:00:00
tags: postgres, introspection, database
---

&nbsp;

![&nbsp;](/images/introspection.png "Detail of Alexander Stirling Calder Introspection (c. 1935)"){.wrap}

In coding, introspection refers to the ability of some systems to query and
expose information on their own structure. Typical examples are being able to
query an object's methods or properties (eg. Python's `___dict___`). 

In a DB system, it typically refers to the mechanism by which schema
information regarding tables, attributes, foreign keys, indices, data types, etc... can be
programmatically queried.

This is useful in many ways, eg:

- Code reuse: making code that can be made schema-agnostic. For example,
  [pgunit](https://github.com/adrianandrei-ca/pgunit), a NUnit-style testing
  framework for postgresql, automatically searches for functions whose name
  start with `test_`.
- Discovery and research of the structure in ill-documented or legacy
  database.

In this article we will explore some options for making use of the
introspection capabilities of PostgreSQL.

## Information schema vs system catalogs

There are two main devices to query information of the objects defined in a
Postgres database. The first one is the information schema, which is defined in
the SQL standard and thus expected to be portable and remain stable, but cannot
provide information about posgres-specific features. As with many aspects of
the SQL standard, there are vendor-specific issues (most notably Oracle does
not implement it out of the box). If you are using introspection as
a part of a library, and do not need to get into postgres-specific
information this approach gives you a better chance for future compatibility
accross RDBMS and even PostgreSQL versions. 

The other approach involves querying the so called [System
Catalogs](https://www.postgresql.org/docs/13/catalogs.html). These are tables
belonging to the `pg_catalog` schema. For example, the `pg_catalog.pg_class`
(pseudo-)table catalogs tables and most everything else that has columns or is
otherwise similar to a table (views, materialized or not...). This approach
is version dependent, but I would be surprised to see major changes in the
near future. 

This is the approach we will be focusing on in this article, because the
tooling and coding ergonomics from PostgreSQL are more convenient, as you will
see in the nexts sections.

## Use the command-line, Luke

The `psql` command-line client is a very powerful and often overlooked utility
(as many other command_-line tools). Typing `\?` after connecting will show a
plethora of commands that let you inspect the DB. What most people do not know,
         though, is that these commands are implemented as regular SQL queries
         to the system catalogs and that **you can actually see the code** just
         by invoking the `psql` client with the `-E` option. For example:

```
PGPASSWORD=<password> psql -E -U <user> -h <host> <db>
```

And then typing for the description of the `pg__catalog.pg_class` table itself:

```
\dt+ pg_catalog.pg_class
```

yields:

```sql
********* QUERY **********
SELECT n.nspname as "Schema",
  c.relname as "Name",
  CASE c.relkind 
    WHEN 'r' THEN 'table' 
    WHEN 'v' THEN 'view' 
    WHEN 'm' THEN 'materialized view' 
    WHEN 'i' THEN 'index' 
    WHEN 'S' THEN 'sequence' 
    WHEN 's' THEN 'special' 
    WHEN 'f' THEN 'foreign table' 
    WHEN 'p' THEN 'partitioned table' 
    WHEN 'I' THEN 'partitioned index' 
  END as "Type",
  pg_catalog.pg_get_userbyid(c.relowner) as "Owner",
  pg_catalog.pg_size_pretty(pg_catalog.pg_table_size(c.oid)) as "Size",
  pg_catalog.obj_description(c.oid, 'pg_class') as "Description"
FROM pg_catalog.pg_class c
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r','p','s','')
      AND n.nspname !~ '^pg_toast'
  AND c.relname OPERATOR(pg_catalog.~) '^(pg_class)$'
  AND n.nspname OPERATOR(pg_catalog.~) '^(pg_catalog)$'
ORDER BY 1,2;
**************************

                        List of relations
   Schema   |   Name   | Type  |  Owner   |  Size  | Description
------------+----------+-------+----------+--------+-------------
 pg_catalog | pg_class | table | postgres | 136 kB |
(1 row)

```

Gives you a quite descriptive (and corner-case complete) template to start your
own code from. For example, in the former query we could replace the
`^(pg_class)$` regex with some other. Bear in mind that this trick is only
helpful with the system catalog approach.

## Regclasses and OIDs

Many objects in the system catalogs have some sort of "unique id" in the form
of an `oid` attribute. It is sometimes convenient to know that you can turn
descriptive names into such `oid`s by casting into the `regclass` data type.

For example, in a somewhat circular turn of events, the attributes of the
catalog table storing attribute information can be queried by name as:

```sql
SELECT attnum, attname, format_type(atttypid, atttypmod) as "Type" 
FROM pg_attribute 
WHERE attrelid = 'pg_attribute'::regclass 
  AND attnum > 0 
  AND NOT attisdropped ORDER BY attnum;
```

In the result of that query, we can see that attrelid should be an `oid`:

```
attnum     |   attname     | Type
-----------+---------------+-----------
         1 | attrelid      | oid
         2 | attname       | name
         ...
        20 | attoptions    | text[]
        21 | attfdwoptions | text[]
```

without the "regclass" cast, querying by name would mean joining with the
`pg_class` and filtering by name. There are other types that will get you an
oid from a string description for other objects (`regprocedure` for procedures,
    `regtype` for types, ...).


## System Catalog Information Functions

Another interesting utility for the `pg_catalog` approach is the ability to
translate definitions into SQL DDL. We saw one of them (`format_type`) in the
previous example, but there are many of them (constraints, function source code ...). 

Just refer to the [section in the manual](https://www.postgresql.org/docs/13/functions-info.html#FUNCTIONS-INFO-CATALOG-TABLE) for more.

## Inspecting arbitrary queries

As a sidenote, it might be useful to know that we can inspect the data types of
any provided query by pretending to turn it into a temporary table. This might
be useful for user-provided queries in external tools (injection caveats apply)...

```sql
CREATE TEMP TABLE tmp AS SELECT 1::numeric, now() LIMIT 0;
```

## Wrapping up

As usual, **good SW practices apply to DB code, too**, and it is easy to isolate any incompatible
code just by defining a clear interface in your library: instead of querying
for the catalog everywhere, define just a set of views or functions that expose
the introspection information to the rest of your code and work as an API. This
way, any future change in system catalogs will not propagate further than those
specific views. For example, if your application needs to know about tables
and attribute data types, instead of querying the catalog from many
places, define a view that works as in interface between the system
catalogs and your code. As an example: 

```sql
CREATE OR REPLACE VIEW table_columns AS
WITH table_oids AS (
      SELECT c.relname, c.oid
      FROM pg_catalog.pg_class c
        LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
      WHERE 
        pg_catalog.pg_table_is_visible(c.oid) AND relkind = 'r'),
    column_types AS (
      SELECT
        toids.relname AS "tablename", 
        a.attname as "column",
        pg_catalog.format_type(a.atttypid, a.atttypmod) as "datatype"
      FROM
        pg_catalog.pg_attribute a, table_oids toids
      WHERE
        a.attnum > 0
        AND NOT a.attisdropped
        AND a.attrelid = toids.oid)
SELECT * FROM column_types;
```

I will be assembling some such utility views I find useful in the future in [this gist](https://gist.github.com/jarnaldich/d5952a134d89dfac48d034ed141e86c5).

