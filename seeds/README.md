## Config files

### CitiesX.txt

Download the appropriate file from http://download.geonames.org/export/dump/
and unzip.

For the format: http://download.geonames.org/export/dump/readme.txt

### Loading data for production

Currently, since the below scripts are a bit unwieldy to run in Heroku,
I simply created a backup of a populated DB and [imported it](https://devcenter.heroku.com/articles/heroku-postgres-import-export#import). 

```sh
# Note that we exclude the `raw` schema, as it's only useful for imports. Migrations
# should be able to reconstitute it, sans data.
> pg_dump -Fc --no-acl --no-owner -Nraw geocode_city_dev > mydb.dump

> heroku pg:backups:restore -a geocode-city 'https://storage.googleapis.com/some-public-url' DATABASE_URL
 ▸    WARNING: Destructive Action
 ▸    This command will affect the app geocode-city
 ▸    To proceed, type geocode-city or re-run this command with --confirm geocode-city

> geocode-city
Starting restore of https://storage.googleapis.com/geocode-city-backups/mydb.dump to postgresql-lively-93516... done

Use Ctrl-C at any time to stop monitoring progress; the backup will continue restoring.
Use heroku pg:backups to check progress.
Stop a running restore with heroku pg:backups:cancel.

Restoring... done
```

Because the DB is tiny (10MB), it took a little under a minute. We should ideally have
a script to run in production to download the latest geonames definitions and load them
appropriately!


### Loading cities for dev


**tl;dr**: only need to run once

1. all scripts in `migrations`, to load the schemata. You should be able to run `stack run -- --migrate` to have the application
  run its migrations; requires a working Haskell environment.
2. `pgloader geonames_cities.load` customized to the cities file in use **make sure to not run the last step until this one has succeeded with the desired data set!** (requires a working
   installation of `pgloader`.)
3. `psql YOUR_DEV_DB < non_city_seeds.sql` -- will load features, countries, continents and admin codes.
4. `psql YOUR_DEV_DB < seeds.sql` -- will load all cities from `raw.geonames` into the normalized `geocode.city` table.

#### On loading raw data:

Once you've downloaded the cities, and ran migrations, you can load a development
set from a geonames dump using [pgloader](https://pgloader.readthedocs.io/en/latest/index.html)

(You can install `pgloader` with homebrew on macOS, with `brew install pgloader`)

There's a sample script, `geonames_cities.load`, which assumes the `cities500.txt` dataset, it should
populate the `raw.geonames` table when running it with `pgloader`:

    pgloader geonames_cities.load


If all goes well, we get this output (using `cities500.txt`):

```sh
> pgloader geonames_cities.load
2021-01-19T03:37:32.008489Z LOG pgloader version "3.6.2"
2021-01-19T03:37:32.010229Z LOG Data errors in '/private/tmp/pgloader/'
2021-01-19T03:37:32.010277Z LOG Parsing commands from file #P"/Users/luis/code/geocode.city/geocode-city-api/seeds/geonames_cities.load"
2021-01-19T03:37:41.481619Z LOG report summary reset
             table name     errors       rows      bytes      total time
-----------------------  ---------  ---------  ---------  --------------
                  fetch          0          0                     0.003s
-----------------------  ---------  ---------  ---------  --------------
       "raw"."geonames"          0     196591    31.7 MB          9.271s
-----------------------  ---------  ---------  ---------  --------------
        Files Processed          0          1                     0.011s
COPY Threads Completion          0          2                     9.317s
-----------------------  ---------  ---------  ---------  --------------
      Total import time          ✓     196591    31.7 MB          9.328s
```

If you prefer the smaller dataset, `cities15000.txt`, it'll take less:

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
