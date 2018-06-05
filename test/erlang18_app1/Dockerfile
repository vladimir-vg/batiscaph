FROM erlang:18
WORKDIR /root

# fetch and compile dependencies that change rarely
# and very slow
COPY rebar3 rebar3
COPY rebar-only-external-deps.config rebar-only-external-deps.config
ENV REBAR_CONFIG "rebar-only-external-deps.config"
RUN ./rebar3 deps
RUN ./rebar3 compile
ENV REBAR_CONFIG "rebar.config"

COPY rebar.config rebar.config

COPY _checkouts _checkouts
COPY src src
RUN ./rebar3 compile
COPY test test

CMD ./rebar3 shell
