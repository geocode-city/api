# geocode-city-api

## Execute  

* Run `stack run` to run the server with the default config (see `Config.hs`). You can override with environment vars: `PORT` and `DATABASE_URL`.
* Run `stack run -- --migrate` to run any pending migrations. We endeavor to write idempotent migrations, so running it
  _shouldn't_ affect an existing schema. 

## Run tests

`stack test`

### SQL Notes

The base schema is based on this article, also part of the Art of PostgreSQL book: https://tapoueh.org/blog/2018/05/postgresql-data-types-point/

(Note: the following examples use the biggest dataset: all cities with >500 population, which is around ~196,000 rows when imported:)

We have some basic indices for trigram operations, which lead to decently fast queries (there's also an index for `alternatenames`, which can take 3x as long, but will have more hits):

```sql
geocode_city_dev=# select name, population from geocode.city where name %> 'teguc' order by population desc, name <-> 'teguc' limit 10;
        name         | population 
---------------------+------------
 Tegucigalpa         |     850848
 Teguise             |      19418
 Tegueste            |      10666
 Costa Teguise       |       7629
 Tegul’det           |       4800
 Teguajinal          |       1117
 Banjar Teguan       |          0
 Ji’ergele Teguoleng |          0
(8 rows)

geocode_city_dev=# explain analyze select name, population from geocode.city where name %> 'teguc' order by population desc, name <-> 'teguc' limit 10;
                                                                     QUERY PLAN                                                                     
----------------------------------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=734.54..734.56 rows=10 width=23) (actual time=0.560..0.563 rows=8 loops=1)
   ->  Sort  (cost=734.54..735.03 rows=197 width=23) (actual time=0.560..0.561 rows=8 loops=1)
         Sort Key: population DESC, ((name <-> 'teguc'::text))
         Sort Method: quicksort  Memory: 25kB
         ->  Bitmap Heap Scan on city  (cost=77.52..730.28 rows=197 width=23) (actual time=0.488..0.549 rows=8 loops=1)
               Recheck Cond: (name %> 'teguc'::text)
               Heap Blocks: exact=6
               ->  Bitmap Index Scan on idx_city_autocomplete_faster  (cost=0.00..77.47 rows=197 width=0) (actual time=0.460..0.460 rows=8 loops=1)
                     Index Cond: (name %> 'teguc'::text)
 Planning Time: 0.121 ms
 Execution Time: 0.593 ms
(11 rows)
```

and for reverse geocoding:

```sql
geocode_city_dev=# select name from geocode.city order by location <-> '(-87.2, 14.06)' limit 5;
    name     
-------------
 Tegucigalpa
 La Paz
 Comayagua
 Danlí
 El Paraíso
(5 rows)

geocode_city_dev=# explain analyze select name from geocode.city order by location <-> '(-87.2, 14.06)' limit 5;
                                                        QUERY PLAN                                                        
--------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=0.25..8.27 rows=1 width=18) (actual time=0.696..0.735 rows=5 loops=1)
   ->  Index Scan using idx_location on city  (cost=0.25..8.27 rows=1 width=18) (actual time=0.692..0.726 rows=5 loops=1)
         Order By: (location <-> '(-87.2,14.06)'::point)
 Planning Time: 10.290 ms
 Execution Time: 0.854 ms
(5 rows)
```
