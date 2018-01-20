# batiscaph

![batiscaph display example](https://github.com/vladimir-vg/batiscaph/raw/master/open_port_and_change_owner.gif)

Batiscaph is a research device that submerges into ocean and explores unknown space.
That's what this tool does -- submerges into Erlang node and observes how programs work in it.

UI ideas for this project could be found here: https://www.figma.com/file/Jz6rhLiGoFbRZOZnoeTBHIvj/Chertezh

Unfortunately I don't have much time to work on this project as I wish I had, because I'm fulltime employee.
I wish I could turn this tool into something very convenient for everyday work.
If you want to help me make it happen please support me at Patreon: https://www.patreon.com/VladimirVG

# How to visualize Common Test runs locally

It worked with my Erlang 20, Clickhouse 1.1.54318, Neo4j 3.0.5 and Node.js v6.11.3.
Haven't tested on other versions, but likely gonna work just fine.

 1. install [Clickhouse](https://clickhouse.yandex/), [Neo4j](https://neo4j.com/) and stable [Nodejs](https://nodejs.org/en/)(required for frontend build)
 2. create empty database in Clickhouse.
 3. specify env variables in Makefile according to your configuration (CLICKHOUSE_DB, CLICKHOUSE_URL, NEO4J_HTTP_URL, HTTP_PORT)
 4. run `npm install .`, This will install React required for frontend
 5. run `./rebar3 get-deps`, `./rebar3 compile` and finally `make shell`.
 6. after getting into erlang shell you need to setup databases.
    Run: `batiscaph:create_tables().`, `batiscaph:prepare_graph_schema().`
 7. after that you should be able to access http://localhost:HTTP_PORT with port you previously specified
 8. run `./rebar3 path --app batiscaph` and copy path to beam files of Batiscaph, gonna need that later
 9. go to directory of a project you want to run tests on
 10. make sure that Common Test run command takes path to batiscaph beam files.
     specify it with `-pa`, like `ct_run ... -pa path/to/batiscaph/ebin ...`
 11. add two lines into `*_SUITE` files that you want to be visualized:
     ```
     -batiscaph_steps(all).
     -compile({parse_transform, batiscaph_suite_transform}).
     ```
     Specifying parse_transform through ct_run args may not work,
     it doesn't guarantee that this transform gonna run last,
     and may conflict with others (like ms_transform).
  12. finally run your ct_run, but also specify `BATISCAPH_NODE=batiscaph@(hostname)` env variable
  13. after run you would see your test run displayed on http://localhost:HTTP_PORT

# Old shell demo

If you want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/batiscaph/tree/demo-07-2017) git tag.