---
title: Upserts in PosgreSQL
date: 2015-08-26T00:00:00
tags: sql, postgresql, upsert
---

Note: most of the content of this article is already in [this blog post](http://www.depesz.com/2012/06/10/why-is-upsert-so-complicated/), which is far more detailed. I'm just writing this for people who want to get directly to the solution and do not care for Perl code.

## What is it?
As its name suggests, it is something that combines the functionality of an insert and an update.

Like when:

 1. You want to update a field of a record if it exists, or insert it with that field value if it does not.
 2. You have a process that inserts incrementally into a DB table, in which you may want to make sure that a certain record exists, and do not want to fail with a duplicate key error if it is already there (you are using the database as a `set` or `bag` data structure).
 3. A generalization of 1: you want to update a field of a record with a function of the old value (eg. incrementing), or assign it a default value if the record does not exist.

## Ok. How can I do this?

Well, first thing is to check your PostgreSQL version. Versions 9.5 will have this functionality built-in (as many other RDBMS do) through the use of the `INSERT ... ON CONFLICT` clause. See the [discussion](https://wiki.postgresql.org/wiki/UPSERT).

At the time of writing this, 9.5 is not available, and anyway many of us are forced to work with legacy versions, so here I will discuss a couple of ways to achive this functionality. Notice that geting this right is not easy, since concurrency and transaction issues must be taken into account. In particular, doing this in your code:

```sql
if (SELECT whatever to check if record exists)
    UPDATE it
else 
   INSERT IT
```

or alternatively,

```sql
if(UPDATE affects one or more records) 
  -- we're done
else 
  INSERT record;
```

Will not work even inside a transaction. The default isolation level for transactions will only guarantee that other connections do not see your intermediate changes and that all of your changes succed or fail at once. (That is, the A(tomic) and I(solated) in ACID). If two different transactions query the db concurrently for a new ID, they might both get the same ID, and the transaction will fail. Setting the transaction level to SERIALIZABLE would work (think of transactions running inside a mutual exclusion region), but the application code should be ready to retry the transaction.

## The general solution(s)

So if you want a general solution that works for all of the 3 cases stated previously, you should probably write a stored procedure in the DB like the [documentation suggests](http://www.postgresql.org/docs/current/static/plpgsql-control-structures.html#PLPGSQL-UPSERT-EXAMPLE):

```sql
CREATE TABLE db (a INT PRIMARY KEY, b TEXT);

CREATE FUNCTION merge_db(key INT, data TEXT) RETURNS VOID AS
$$
BEGIN
    LOOP
        -- first try to update the key
        UPDATE db SET b = data WHERE a = key;
        IF found THEN
            RETURN;
        END IF;
        -- not there, so try to insert the key
        -- if someone else inserts the same key concurrently,
        -- we could get a unique-key failure
        BEGIN
            INSERT INTO db(a,b) VALUES (key, data);
            RETURN;
        EXCEPTION WHEN unique_violation THEN
            -- Do nothing, and loop to try the UPDATE again.
        END;
    END LOOP;
END;
$$
LANGUAGE plpgsql;

SELECT merge_db(1, 'david');
SELECT merge_db(1, 'dennis');
```

This solution has the advantage of delegating db-specific code into the DB and simplifying the SQL queries on the application side.

### Other options

 - Coding in the application instead of the DB. Doing the same in client code is trickier and depends on the language and database acess layer and involves the use of SAVEPOINTS .Again, check [the original article](http://www.depesz.com/2012/06/10/why-is-upsert-so-complicated/) for a Perl example.
 - Locking a table. It is safe but slow.
 - Using [advisory locks](http://www.postgresql.org/docs/current/interactive/explicit-locking.html#ADVISORY-LOCKS) will work and have good performance, but can lead to the same kind of problems that other in-process concurrency primitives (like mutexes, semaphores) have (eg. forgetting to acquire a lock). These problems could be minimized by a sound application db access layer.
 
## Partial solutions

If you know in advance that your process will not run concurrently, then all of the above can be greatly simplified. For example, if just want to make sure that a record exists in the db (and not fail with a duplicate key entry the second time), a combination of an INSERT and a SELECT will work, making use of the fact that INSERTS will accept a sub-SELECT instead of a VALUES clause:

```sql
INSERT INTO TEST (whatever, counter)
    SELECT 123, 1 WHERE NOT EXISTS (SELECT * FROM test WHERE whatever = 123);
```

... that is more concise, but not that different from checking the DB state inside the application code.


[see](http://www.the-art-of-web.com/sql/upsert/)


