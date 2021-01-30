FROM haskell:8.8.4 as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

# Need postgresql-server-dev to compile postgresql packages
# Create the file repository configuration:
# From: https://docs.docker.com/engine/examples/postgresql_service/
# And: https://www.postgresql.org/download/linux/debian/

# RUN echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list
# This one's from:
#RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8

#RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -

# Install the latest version of PostgreSQL.
# If you want a specific version, use 'postgresql-12' or similar instead of 'postgresql':
RUN apt-get update && apt-get install -y postgresql-server-dev-12


COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
# compile some heavy-hitters with one thread to not overwhelm poor ol' docker
RUN stack build --system-ghc --only-dependencies -j1 servant servant-swagger
RUN stack build --system-ghc --only-dependencies

# ---- SECOND LAYER ---

FROM haskell:8.8.4 as build

RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -

# Install the latest version of PostgreSQL.
# If you want a specific version, use 'postgresql-12' or similar instead of 'postgresql':
RUN apt-get update && apt-get install -y libpq-dev

COPY --from=dependencies /root/.stack /root/.stack
RUN mkdir -p /opt/geocode-city-api/bin

# Copy all (source, config, static) into the workdir
COPY . /opt/geocode-city-api
WORKDIR /opt/geocode-city-api

## now build the targets. 
RUN stack --system-ghc build
RUN stack --local-bin-path /opt/geocode-city-api/bin install

# -- FINAL LAYER ---

# Using multi-stage builds:
# https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#use-multi-stage-builds
FROM debian:buster

# latest (buster) debian doesn't have gnupg or curl!
RUN apt-get update
RUN apt-get install -y --no-install-recommends gnupg curl


RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ buster-pgdg main" > /etc/apt/sources.list.d/pgdg.list \
  && curl -ksSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -

# Install the latest version of PostgreSQL.
# If you want a specific version, use 'postgresql-12' or similar instead of 'postgresql':
RUN apt-get update && apt-get install -y libpq-dev postgresql-client-12

# Add user and setup path (for local testing, ignored by Heroku)
RUN adduser geocode-city-api
USER geocode-city-api

COPY --from=build /opt/geocode-city-api/migrations /opt/geocode-city-api/migrations
COPY --from=build /opt/geocode-city-api/bin /opt/geocode-city-api

WORKDIR /opt/geocode-city-api

CMD [ "/opt/geocode-city-api/geocode-city-api-exe" ]
