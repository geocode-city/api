# geocode-city-api

![build](https://github.com/geocode-city/api/workflows/Haskell%20CI/badge.svg)


## Deployment

I personally use the [Heroku Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime). For this repository, pushes to the `main` branch will automatically
deploy to Heroku. To manually generate a Docker image and load it locally, you can run:

    docker load < $(nix-build ./nix/docker.nix)

**NOTE**: the above requires a linux environment. On my Mac, I use [linuxkit](https://github.com/nix-community/linuxkit-nix).

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

To provide repeatable builds, I use `nix`. To build the executable, you can either enter a `nix-shell`,
which provides a development environment (`cabal` and `haskell-language-server` are included,) or
build the project with `nix-build ./nix/release.nix`.

### Execute  

* Run `nix-shell --run 'cabal new-run'` to run the server with the default config (see `Config.hs`). You can override with environment vars: `PORT` and `DATABASE_URL`.
* Run `nix-shell --run 'geocode-city-api-exe --migrate'` to run any pending migrations. We endeavor to write idempotent migrations, so running it
  _shouldn't_ affect an existing schema. 

### Run tests

`nix-shell --run 'cabal test'`
