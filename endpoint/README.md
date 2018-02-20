# batiscaph

![batiscaph display example](https://github.com/vladimir-vg/batiscaph/raw/demo-01-2018/open_port_and_change_owner.gif)

Batiscaph is a research device that submerges into ocean and explores unknown space.
That's what this tool does -- submerges into Erlang node and observes how programs work in it.

# How to setup batiscaph locally

It worked with my Erlang 20, Clickhouse 1.1.54318, Neo4j 3.0.5 and Node.js v6.11.3.
Haven't tested on other versions, but likely gonna work just fine.

 1. install [Clickhouse](https://clickhouse.yandex/), [Neo4j](https://neo4j.com/) and stable [Nodejs](https://nodejs.org/en/)(required for frontend build)
 2. create empty database in Clickhouse.
 3. make sure you started Neo4j, `sudo service neo4j start` or smth like that.
 4. specify env variables in Makefile according to your configuration (CLICKHOUSE_DB, CLICKHOUSE_URL, NEO4J_HTTP_URL, HTTP_PORT)
 5. run `npm install .`, This will install React required for frontend
 6. run `./rebar3 get-deps`, `./rebar3 compile` and finally `make shell`.
 7. after getting into erlang shell you need to setup databases.
    Run: `batiscaph:create_tables().`, `batiscaph:prepare_graph_schema().`
 8. after that you should be able to access http://localhost:HTTP_PORT with port you previously specified

# How to run sample test suite

 9. just run `make ct`
 10. open web UI at http://localhost:HTTP_PORT, hover on last session url

# How to visualize Common Test runs locally

 9. run `./rebar3 path --app batiscaph` and copy path to beam files of Batiscaph, gonna need that later
 10. go to directory of a project you want to run tests on
 11. make sure that Common Test run command takes path to batiscaph beam files.
     Specify it with `-pa`, like `ct_run ... -pa path/to/batiscaph/ebin ...`
 12. add two lines into `*_SUITE` files that you want to be visualized:
     ```
     -batiscaph_steps(all).
     -compile({parse_transform, batiscaph_suite_transform}).
     ```
     Specifying parse_transform through ct_run args may not work,
     it doesn't guarantee that this transform gonna run last,
     and may conflict with others (like ms_transform).
  13. finally run your ct_run, but also specify `BATISCAPH_NODE=batiscaph@$(hostname)` env variable
  14. after run you would see your test run displayed on http://localhost:HTTP_PORT

# How to connect to node and start visual shell there

 9. make sure that node is accessible.
    Run `erl -sname test1` and then `net_adm:ping('YOUR_NODE').`.
    If it responds with `pong` then it is accessible.
 10. open web UI on http://localhost:HTTP_PORT, enter YOUR_NODE in text input, click "connect".

# Old shell demo from July 2017

If you want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/batiscaph/tree/demo-07-2017) git tag.
