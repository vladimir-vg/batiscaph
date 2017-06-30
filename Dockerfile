FROM ubuntu:16.04
RUN apt-get update
RUN apt-get install -y curl
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN curl -sL https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb > erlang-solutions_1.0_all.deb
RUN dpkg -i ./erlang-solutions_1.0_all.deb
RUN apt-get update
RUN apt-get install -y nodejs git build-essential erlang-nox=1:20.0-1
ADD espace /opt/espace/espace
ADD erunner /opt/espace/erunner

WORKDIR /opt/espace/erunner
RUN ./rebar3 compile

WORKDIR /opt/espace/espace
RUN ./rebar3 get-deps
RUN ./rebar3 compile
RUN npm install .

CMD make