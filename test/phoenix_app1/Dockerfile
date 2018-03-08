FROM elixir:1.4
WORKDIR /root

ENV MIX_ENV prod
RUN mix local.hex --force
RUN mix local.rebar --force

COPY mix.* ./
# vision_probe dependency is not included yet
RUN mix deps.get --only prod
RUN mix deps.compile

# changes rarely
COPY before-test-run.sh before-test-run.sh

# now add vision_probe dep
COPY _checkouts _checkouts
RUN mix deps.get --only prod
RUN mix deps.compile

# now add source
COPY config config
COPY lib lib
COPY priv priv

RUN mix compile
