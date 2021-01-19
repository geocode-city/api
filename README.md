# geocode-city-api

## Execute  

* Run `stack exec -- geocode-city-api-exe` to see "We're inside the application!"
* With `stack exec -- geocode-city-api-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`

### SQL Notes

We have some basic indices for trigram operations, which lead to decently fast queries (there's also an index for `alternatenames`, which can take 3x as long, but will have more hits):

```sql
geocode_city_dev=# select name, population from geocode.city where name %> 'tegus' order by population desc, name <-> 'tegus' limit 10;
    name     | population 
-------------+------------
 Tegucigalpa |     850848
 Teguise     |      19418
(2 rows)


geocode_city_dev=# explain analyze select * from geocode.city where name %> 'tegus' order by population desc, name <-> 'tegus' limit 10;
                                                                    QUERY PLAN                                                                    
--------------------------------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=94.37..94.39 rows=10 width=235) (actual time=11.488..11.490 rows=2 loops=1)
   ->  Sort  (cost=94.37..94.43 rows=25 width=235) (actual time=11.487..11.488 rows=2 loops=1)
         Sort Key: population DESC, ((name <-> 'tegus'::text))
         Sort Method: quicksort  Memory: 27kB
         ->  Bitmap Heap Scan on city  (cost=4.47..93.83 rows=25 width=235) (actual time=11.468..11.479 rows=2 loops=1)
               Recheck Cond: (name %> 'tegus'::text)
               Heap Blocks: exact=2
               ->  Bitmap Index Scan on idx_city_autocomplete_fast  (cost=0.00..4.46 rows=25 width=0) (actual time=11.449..11.449 rows=2 loops=1)
                     Index Cond: (name %> 'tegus'::text)
 Planning Time: 0.130 ms
 Execution Time: 11.534 ms
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
---------------------------------------------------------------------------------------------------------------------------------
 Limit  (cost=0.28..1.51 rows=5 width=18) (actual time=0.100..0.137 rows=5 loops=1)
   ->  Index Scan using idx_location on city  (cost=0.28..6067.10 rows=24541 width=18) (actual time=0.099..0.135 rows=5 loops=1)
         Order By: (location <-> '(-87.2,14.06)'::point)
 Planning Time: 0.098 ms
 Execution Time: 0.159 ms
(5 rows)
```
