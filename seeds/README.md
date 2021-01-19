## Config files

### CitiesX.txt

Download the appropriate file from http://download.geonames.org/export/dump/
and unzip.

For the format: http://download.geonames.org/export/dump/readme.txt


### Loading cities for dev


**tl;dr**: only need to run once

- all scripts in `migrations`, to load the schemata
- `pgloader geonames_cities.load` customized to the cities file in use **make sure to not run the last step until this one has succeeded with the desired data set!**
- `psql YOUR_DEV_DB < non_city_seeds.sql` -- will load features, countries, continents and admin codes.
- `psql YOUR_DEV_DB < seeds.sql` -- will load all cities
in the normalized `geocode.city` table.

#### 1. Load raw data:

Once you've downloaded the cities, and ran migrations, you can load a development
set from a geonames dump using [pgloader](https://pgloader.readthedocs.io/en/latest/index.html)

(You can install `pgloader` with homebrew on macOS, with `brew install pgloader`)

There's a sample script, `geonames_cities.load`, which assumes the `15000` dataset, it should
populate the `raw.geonames` table when running it with `pgloader`:

    pgloader geonames_cities.load


If all goes well, we get this output:

```sh
> pgloader geonames_cities.load
2021-01-19T01:42:11.009698Z LOG pgloader version "3.6.2"
2021-01-19T01:42:11.011372Z LOG Data errors in '/private/tmp/pgloader/'
2021-01-19T01:42:11.011417Z LOG Parsing commands from file #P"/Users/luis/code/geocode.city/geocode-city-api/config/geonames_cities.load"
2021-01-19T01:42:13.020800Z LOG report summary reset
             table name     errors       rows      bytes      total time
-----------------------  ---------  ---------  ---------  --------------
                  fetch          0          0                     0.003s
-----------------------  ---------  ---------  ---------  --------------
       "raw"."geonames"          0      24541     5.8 MB          1.718s
-----------------------  ---------  ---------  ---------  --------------
        Files Processed          0          1                     0.011s
COPY Threads Completion          0          2                     1.778s
-----------------------  ---------  ---------  ---------  --------------
      Total import time          ✓      24541     5.8 MB          1.788s
```
