FROM erlang:17
WORKDIR /root

# fetch and compile dependencies that change rarely
# and very slow
COPY rebar3 rebar3
COPY rebar-only-external-deps.config rebar-only-external-deps.config
ENV REBAR_CONFIG "rebar-only-external-deps.config"
RUN ./rebar3 deps
RUN ./rebar3 compile
ENV REBAR_CONFIG "rebar.config"

COPY before-test-run.sh before-test-run.sh
COPY rebar.config rebar.config

COPY _checkouts _checkouts
COPY src src
RUN ./rebar3 compile

CMD ./rebar3 shell
