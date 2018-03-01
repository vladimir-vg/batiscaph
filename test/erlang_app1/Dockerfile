FROM erlang:17
WORKDIR /root

COPY before-test-run.sh before-test-run.sh

# adding local version of vision probe
# since there are no other deps in this project
# no need to split vision_probe build
# and other dependencies build
COPY _checkouts _checkouts
COPY rebar3 rebar3
COPY rebar.config rebar.config
COPY src src

RUN ./rebar3 compile

CMD ./rebar3 shell
