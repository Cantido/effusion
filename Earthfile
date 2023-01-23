VERSION 0.6

ARG MIX_ENV=dev

all:
  BUILD +test
  BUILD +docker

get-deps:
  FROM hexpm/elixir:1.13.0-erlang-25.0-debian-bullseye-20210902-slim
  COPY mix.exs .
  COPY mix.lock .

  RUN mix do local.rebar --force, local.hex --force
  RUN mix deps.get

compile-deps:
  FROM +get-deps
  RUN MIX_ENV=$MIX_ENV mix deps.compile

build:
  FROM +compile-deps

  COPY --dir lib/ config/ .

  RUN MIX_ENV=$MIX_ENV mix compile

test:
  FROM --build-arg MIX_ENV=test +build

  COPY --dir test/ .

  COPY docker-compose.yml .
  WITH DOCKER --compose docker-compose.yml
    RUN MIX_ENV=test mix do setup, test
  END

build-release:
  FROM +build

  RUN MIX_ENV=$MIX_ENV mix release

  SAVE ARTIFACT _build AS LOCAL ./_build

docker:
  FROM alpine
  WORKDIR /app
  RUN apk add --no-cache --update bash openssl

  COPY +build-release/_build/$MIX_ENV/rel/effusion .

  ENV MIX_ENV=$MIX_ENV

  CMD ["/app/bin/effusion", "start"]

  SAVE IMAGE --push cantido/effusion
