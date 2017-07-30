If you want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/espace/tree/demo-07-2017) git tag.

This branch is for ongoing development.

# Development setup

Unlike in demo, espace do not stores events in csv files anymore. Now it uses database for it.
You need to install clickhouse (column oriented dbms) and neo4j (graph database).

Specify neo4j url (`NEO4J_HTTP_URL`) with your login:pass if needed.
You may take look at `espace:read_config/0` function for better understanding.

Also you need to install babel to use web interface in development mode:

    npm install .

To start server just type `make`.

# Current goal

Provide shell UI that encourages exploration of remote erlang node. Provide a map of processes and their relationships.

Allow users to inspect processes and have tracing automatically enabled.

Record whole session to database, for later inspection. Might be very useful for referring to bugs in tickets.

# How it supposed to work

Target erlang have no prerequisites, or have small client library that allows espace to connect to it.

After connection espace uploads client modules to remote node, it starts tracer and shell.

Espace collects events from remote node (including shell IO), stores it to database, and displays gathered information in browser.

Espace collects not only traced events (`spawn`, `link`, `exit`) but also calls that may tell more info about explored system. For example `erlang:whereis/1` will add info about registered name to pid, even if this process was not traced and we had no information about it. Same for `erlang:process_info/1,2`. Therefore user can explore system using familiar calls and have returned information recorded and displayed on the map.

Collected events produce graph (neo4j) that gonna be used for querying.

After user focuses on any process it's starts being traced, tracing stops if User unfocus it. User may pin tracing for this process, or even enable tracing for the whole tree, all processes spawned by current.

Messages between processes are stored if there not too many of them, and they aren't that big. Need to create throttling mechanism to ignore some of the messages (but still count them).

Having many processes and events recorded we should be able to query information. If we need to search for a process, not an event, then best way to do it is to construct a pattern of processes and their relationships. A pattern-matching query interface.

It might be useful to have several queries in one workspace, and having interlapsing results highlighted.

# Process tracing levels

 0. Process was mentioned by some other process. No tracing is enabled. Some mentions (but not every) are followed by `erlang:is_process_alive/1` call. If it is alive, it displayed as gray box (according to timestamp) on timeline. If it's dead -- displayed as X. One process may have several mentions connected on timeline.

 0. If mentioned process was selected by user, it turns into "explored" state. After that `exit`, `spawn`, `link` and `unlink` events are traced for this process. Same tracing applies to all process links recursively if process does not have trap_exit enabled. Such strategy will expand linked "islands" of processes, stopping at trap_exit-ed processes (usually supervisors, but also others). Also during exploration some other information gathered, if this process responds to OTP conventions, for example `$ancestors`.
 
 It's important to remember which process was selected by user. If linked process unlinks from that user-selected process, then tracing for it should be stopped. This policy is needed to avoid tracing being trasmitted to too many processes, outside of "linked island" that we were interested in.

 0. Process was explicitly enabled to listen additional info. Currently it means collecting messages that were sent by this process. If number of message events exceeds some limit (process-wise and node-wise per second), then one summary event is generated instead. Also it might be great to trace work with ets tables, at least to count updates, inserts and deletions and to which table.

# Other tracing

Usually very few ports are used in apps. Probably it's safe to trace them all for `closed`, `open`, `link` and `unlink`, just like "explored" state for processes. But without having it recursively applied to linked processes. Just mention linked processes.

Probably it's also safe to trace all `ets:new/2`, `ets:give_away/3` and `ets:setopts/2` globally. Collect information about ets tables just like about processes.

If there would be a way to trace all `register`/`unregister` events, without tracing all processes in the system -- would love to have that.

# Shell commands and map effects

 * `[whereis(A) || A <- registered()].`. Will mention and put on the map all processes that were registered on the node.

# TODO

Remote tracing code.

- [ ] Implement fetching info for mentioned processes.
- [ ] Implement "explored" tracing state for processes, propagate it recursively for links.
- [ ] Trace globally open/close, link/unlink events for ports.
- [ ] Trace globally ets new/delete and give_away events. Watch for table controlling processes.

Events collecting code.

- [ ] Build graph nodes for ports and ets events.

Frontend code.

- [ ] Write new visualization code using d3 instead of plain React SVG. React is not convenient for animations that we gonna need in future.
- [ ] Figure out how to organize complicated map code. Try to keep it described as components, while having it rendered as several layers.
- [ ] Figure out better representation for nodes on map and their events. Generate coordinates from it. Allow to regroup, sort or drag different objects on the map.
- [ ] Use proper router that supports html5-history (get rid of demo routing bugs).
