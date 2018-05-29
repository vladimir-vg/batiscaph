# This file is not used

# FROM erlang:20
# WORKDIR /root
# 
# # # add java8 repo, required for Debian 8 Jessie
# # RUN echo "deb http://httpredir.debian.org/debian jessie-backports main" | tee -a /etc/apt/sources.list.d/jessie-backports.list
# # 
# # # add neo4j repo
# # RUN wget -O - https://debian.neo4j.org/neotechnology.gpg.key | apt-key add -
# # RUN echo 'deb http://debian.neo4j.org/repo stable/' | tee -a /etc/apt/sources.list.d/neo4j.list
# # 
# # RUN apt-get update
# # RUN apt-get -t jessie-backports install ca-certificates-java -y
# # RUN apt-get install neo4j=3.3.2 openjdk-8-jre-headless -y
# # RUN update-java-alternatives --jre-headless --set java-1.8.0-openjdk-amd64
# # 
# # # installing clickhouse
# # ADD dist dist
# # RUN dpkg -i dist/*.deb
# 
# ENV BATISCAPH_ENDPOINT_CLICKHOUSE_DB batiscaph_test
# ENV BATISCAPH_ENDPOINT_CLICKHOUSE_URL http://127.0.0.1:8123/
# COPY before-test-run.sh before-test-run.sh
# 
# # first add only rebar config and download deps,
# # to make stage of bulding image reusable.
# # to avoid downloading deps on every source change
# COPY rebar3 rebar3
# COPY rebar-without-probe.config rebar.config
# RUN ./rebar3 deps
# RUN ./rebar3 compile
# 
# COPY priv/pg_queries.sql priv/pg_queries.sql
# COPY priv/clk_queries.sql priv/clk_queries.sql
# COPY src src
# RUN ./rebar3 compile
# 
# # CMD ./rebar3 shell
