FROM elixir:1.6.1
WORKDIR /root

ENV MIX_ENV prod
RUN mix local.hex --force
RUN mix local.rebar --force

COPY mix.* ./
RUN mix deps.get --only prod
RUN mix deps.compile

COPY before-test-run.sh before-test-run.sh

COPY assets assets
COPY config config
COPY lib lib
COPY priv/gettext priv/gettext
COPY priv/repo priv/repo

RUN mix compile
