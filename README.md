If you're want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/espace/tree/demo-07-2017) git tag.

# Current goal

Provide shell UI that ecourages exploration of remote erlang node. Provide a map of processes and their relationships.

Allow users to inspect processes and have tracing automatically enabled for specific processes.

Record whole session to database, for later inspection. Might be very useful for referring to bugs in tickets.

# How it supposed to work

Target erlang have no prerequisites, or have small client library that allows espace to connect to it.

After connection espace uploads client modules to remote node, it starts tracer and shell.

Espace collects events from remote node (including shell IO), stores it to database, and displays gathered information in browser.

Espace collects not only traced events (spawn, link, exit) but also calls that may tell more info about explored system. For example `erlang:whereis/1` will add info about registered name to pid, even if this process was not traced and we had no information about it. Same for `erlang:process_info/1,2`. Therefore user can exlore system using familiar calls and have returned information recorded and displayed on the map.

Collected events produce graph (neo4j) that gonna be used for querying.

After user focuses on any process it's starts being traced, tracing stops if User unfocus it. User may pin tracing for this process, or even enable tracing for the whole tree, all processes spawned by current.

Messages between processes are stored if there not too many of them, and they aren't that big. Need to create throttling mechanism to ignore some of the messages (but still count them).

Having many processes and events recorded we should be able to query information. If we need to search for a process, not an event, then best way to do it is to construct a pattern of processes and their relationships. A pattern-matching query interface.

It might be useful to have several queries in one workspace, and having interlapsing results highlighted.
