FROM erlang:20
WORKDIR /root

# first add only rebar config and download deps,
# to make stage of bulding image reusable.
# to avoid downloading deps on every source change
COPY rebar.config rebar.config
COPY rebar3 rebar3
RUN ./rebar3 get-deps
RUN ./rebar3 compile

COPY src src
RUN ./rebar3 compile

CMD ./rebar3 shell
