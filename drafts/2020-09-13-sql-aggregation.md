---
title: Advanced SQL Aggregation
date: 2020-09-13T:00:00
tags: sql, postgres
---

Aggregation is one of the most commonly used features of SQL. In general, it
boils down to:

1. Splitting the table among different sets of records, all sharing
some common criteria (most commonly the same value for an attribute or set of
attributes, eg ```sql GROUP BY```).

2. Computing some summary function (aggregation) for each of this groups.

3. (optional) Further filtering the result of the binary function ```sql HAVING```.

4. Building the query result. In some cases we will only be interested in the "summary rows" (one result per group),
   while others can merge the group summary alongside the original records.

## Sample data

All along this tutorial we will work with the following data definition, which
emulates a very crude visit log for some website:

```sql
CREATE TABLE visit_log (
  url TEXT NOT NULL,
  date DATE NOT NULL,
  hits integer NOT NULL default 0,
  CONSTRAINT visits_pk PRIMARY KEY (url, date)
);

INSERT INTO visit_log VALUES
	('/one', '2020/11/01', 2),
    ('/one', '2020/11/02', 2),
    ('/one', '2020/11/03', 100),
	('/two', '2020/11/01', 90),
    ('/two', '2020/11/02', 90),
    ('/two', '2020/11/03', 90)
    ;
```

I encourage you to follow along this tutorial in [Db
Fiddle](https://www.db-fiddle.com/) by forking the following
[DDL](https://www.db-fiddle.com/f/3DA3J6nqcCPnhDGB52Nqn6/0).




[](https://www.db-fiddle.com/f/jQf6NDVPVzjmcm3dyfzGt3/5)
