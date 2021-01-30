# geocode-city-api

## Deployment

I personally use the [Heroku Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime); but instead of using [their base images](https://devcenter.heroku.com/articles/heroku-20-stack#heroku-20-docker-image) (which are rather large,)
you'll see in the `Dockerfile` that I stubbornly use `haskell` base images to build dependencies,
and `debian:buster` for the final image -- which requires installing Postgres libraries at every step, and a very questionable `curl -k...`. I did however take some inspiration from the heroku [image Dockerfiles](https://github.com/heroku/stack-images/blob/main/heroku-20/setup.sh)

On a solidly average machine (8GB ram, 3 allocated to Docker,) a build takes ~30 mins from scratch, but under a minute if all dependencies have already been built and we're just recompiling the executables. I try to keep dependencies small, but there's at least two heavy hitters: `lens` and `swagger2`: Docker with only 2GB was running out of memory trying to compile these behemoths, even
with the `-j1` flag sent to Stack!

For my setup, these commands do the trick:

```sh
heroku container:push web -a geocode-city
heroku container:release web -a geocode-city
```

If you choose to use heroku, the `Dockerfile` should get you the above to work, too.

### Datastores

In heroku, I use the `hobby-dev` Postgres add-on, and the free `redis` addon. One thing to note,
is that for the hyperloglog entries to "fall off" as we reach max memory, I'm using the `allkeys-lru` eviction policy, set with:

```sh
heroku redis:maxmemory REDIS-NAME --policy allkeys-lru -a geocode-city
```

#### Database migrations (Heroku specific)

I _should_ set up a [`release` phase](https://devcenter.heroku.com/articles/container-registry-and-runtime#release-phase) to [run migrations automatically](https://devcenter.heroku.com/articles/release-phase). However, I'd rather not have that overhead for something that
happens somewhat seldom, so I instead use a [one-off dyno](https://devcenter.heroku.com/articles/container-registry-and-runtime#one-off-dynos):

```sh
> heroku run bash -a geocode-city
Running bash on ⬢ geocode-city... up, run.7528 (Hobby)
~ $ pwd
/opt/geocode-city-api
~ $ ls
geocode-city-api-exe  migrations
~ $ ./geocode-city-api-exe --migrate
Initializing schema
NOTICE:  relation "schema_migrations" already exists, skipping
Ok:	202101182025_geonames_raw.sql
Ok:	202101182130_non_city_tables.sql
Ok:	202101182140_city_table.sql
Ok:	202101182150_ft_and_indexes.sql
Ok:	202101191900_api_keys_table.sql
Ok:	202101201800_materialize_autocomplete.sql
Ok:	202101231600_api_quotas.sql
Execute:	202101301530_full_text_autocomplete.sql
All migrations ran.
```

## Development

### Execute  

* Run `stack run` to run the server with the default config (see `Config.hs`). You can override with environment vars: `PORT` and `DATABASE_URL`.
* Run `stack run -- --migrate` to run any pending migrations. We endeavor to write idempotent migrations, so running it
  _shouldn't_ affect an existing schema. 

### Run tests

`stack test`
