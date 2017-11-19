# batiscaph

![Submersible MIR-2](https://pp.userapi.com/c639530/v639530374/4fcba/8ykO3N012hA.jpg)

Batiscaph is a research device that submerges in ocean and explores unknown space.
That's what this tool does -- submerges into Erlang node and observes how it works.

Design ideas for this project could be found here: https://www.figma.com/file/Jz6rhLiGoFbRZOZnoeTBHIvj/Chertezh

# Old demo

If you want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/espace/tree/demo-07-2017) git tag.

This branch is for ongoing development.

# Goals

 * Provide shell UI that encourages exploration of remote erlang node. Provide a map of processes and their relationships.
 * Allow users to inspect processes and have tracing automatically enabled.
 * Record whole session to database, for later inspection and sharing. Should be good for referring to bugs in tickets.

# Design ideas

 * Make it work on a loaded production system.
 * Use minimal set of dependencies when possible. Don't expect perfect environment.
 * Keep UI clean and simple, which everyday working tool should have.

# Development setup

You need to install clickhouse (column oriented dbms) and neo4j (graph database).

Specify neo4j url (`NEO4J_HTTP_URL`) with your login:pass if needed.
You may take look at `batiscaph_app:read_config/0` function for better understanding.

Also you need to install babel to use web interface in development mode:

    npm install .

To start server just type `make`.

# How it supposed to work

Target erlang node have no prerequisites, or have small client library that allows batiscaph to connect to it.
After connection batiscaph uploads client modules to remote node, starts tracer and shell.

Batiscaph collects events from remote node (including shell IO), stores it to database, and displays gathered information in browser.

Batiscaph collects not only traced events (`spawn`, `link`, `exit`) but also calls that may tell more info about explored system. For example `erlang:whereis/1` will add info about registered name to pid, even if this process was not traced and we had no information about it. Same for `erlang:process_info/1,2`. Therefore user can explore system using familiar calls and have returned information recorded and displayed on the map.

Collected events produce graph (neo4j) that gonna be used for querying.

Messages between processes are stored if there not too many of them, and they aren't that big. Need to create throttling mechanism to ignore some of the messages (but still count them).

Having many processes and events recorded, we should be able to query information. If we need to search for a process, not an event, then best way to do it is to construct a pattern of processes and their relationships. A pattern-matching query interface.

It might be useful to have several queries in one workspace, and having interlapsing results highlighted.

# Features to implement, tasks to do

 - [ ] collect and display info about, creating, deleting, owning and transferring (ets:give_away/3) ETS tables.
 - [ ] workaround execution of record expressions in erl_eval (batiscaph_steps).
 - [ ] create link-nodes that connected to all linked processes.
       This required to make it clear which processes going to die, if any of them is dead.
       Unlink, link and trap_exit create new link-nodes.
 - [ ] run Erlang/OTP tests using batiscaph_steps.
       Should reveal many bugs, and also good way to observe correctness of display.
 - [ ] display expression for mentioned processes in steps.
 - [ ] fix bug when only first line of multiline expression is copied when run through steps.
 - [ ] find out how to organize JavaScript code that works with delta and tree. objtree.js is kinda messy now.

# Global ideas to think about

 * When and how display information about loaded modules in system.
   Might be useful to quickly check what modules and functions are available exactly on that system.

 * It would be great to create a mechanism that allows to record communication of the port, and then reproduce it,
   by creating a new port. How it would look like: you reproduce certain request in browser, see a problem.
   Then you start a bot that tried to reproduce this request from time to time, while you debugging your system
   and change code. If this mechanism could be generalized, and bots made smarter, this might become crucial part
   of development workflow.

 * Being able to make several queries to batiscaph, and having them displayed together with overlaps.
   Query is actually a subscription, it updates if more info is available.

 * Might be good to provide interface to write complicated tracing rules.
   Just like in shell, but simplified and convenient as possible. But no abstractions on top, just as it is in runtime.

# Other technical details

 * Clickhouse team recommends to store timestamps with more than second precision in two columns: https://github.com/yandex/ClickHouse/issues/525#issuecomment-282576661, that's why we have at_s and at_mcs. But it's much easier to operate with one column outside of Clickhouse. Neo4j and JavaScript limit integers to be < 2^53, and full microsecond timestamp fits in it. So one `at` field is used outside of Clickhouse.
