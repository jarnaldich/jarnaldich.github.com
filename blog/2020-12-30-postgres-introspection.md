---
title: Postgres introspection
date: 2020-12-30T:00:00
tags: postgres, metaprogramming
---

By introspection I mean checking the returned fields or data types of a particular query or table

## Information schema vs system catalogs

## Inspecting arbitrary queries

```sql
CREATE TEMP TABLE tmp AS SELECT 1::numeric, now() LIMIT 0;
```

https://dba.stackexchange.com/questions/75015/query-to-return-output-column-names-and-data-types-of-a-query-table-or-view
