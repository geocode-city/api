FROM haskell:8.8.4 as dependencies

RUN mkdir /opt/build
WORKDIR /opt/build

COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
# RUN stack build --system-ghc --only-dependencies -j1 servant
RUN stack build --system-ghc --only-dependencies

# ---- SECOND LAYER ---

FROM haskell:8.8.4 as build

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
FROM debian:latest

# Add user and setup path (for local testing, ignored by Heroku)
RUN adduser geocode-city-api
USER geocode-city-api

COPY --from=build /opt/geocode-city-api/bin /opt/geocode-city-api
COPY --from=build /opt/geocode-city-api/config /opt/geocode-city-api/config
COPY --from=build /opt/geocode-city-api/static /opt/geocode-city-api/static

WORKDIR /opt/geocode-city-api

CMD [ "/opt/geocode-city-api/geocode-city-api-exe" ]
