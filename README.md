# geocode-city-api

## Deployment

I personally use the [Heroku Container Registry](https://devcenter.heroku.com/articles/container-registry-and-runtime); but instead of using their base images (which are rather large,)
you'll see in the `Dockerfile` that I stubbornly use `haskell` base images to build dependencies,
and `debian:buster` for the final image -- which requires installing Postgres libraries at every step, and a very questionable `curl -k...`. 

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


## Development

### Execute  

* Run `stack run` to run the server with the default config (see `Config.hs`). You can override with environment vars: `PORT` and `DATABASE_URL`.
* Run `stack run -- --migrate` to run any pending migrations. We endeavor to write idempotent migrations, so running it
  _shouldn't_ affect an existing schema. 

### Run tests

`stack test`
