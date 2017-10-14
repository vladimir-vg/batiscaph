# batiscaph

![Submersible MIR-2](https://pp.userapi.com/c639530/v639530374/4fcba/8ykO3N012hA.jpg)

Batiscaph is a research device that submerges in ocean and explores unknown space.

That's what this tool does -- submerges into Erlang node and observes how it works.

# Old demo

If you want to locally setup demo [that was described in my blog](http://vladimir-vg.me/erlang-shell-visualization-demo/), please check out [demo-07-2017](https://github.com/vladimir-vg/espace/tree/demo-07-2017) git tag.

This branch is for ongoing development.

# Design ideas

 * Make it work on a loaded production system.
 * Use minimal set of dependencies when possible. Don't expect perfect environment.
 * Keep UI clean and simple, which everyday working tool should have.

# Development setup

Unlike in demo, batiscaph do not stores events in csv files anymore. Now it uses database for it.
You need to install clickhouse (column oriented dbms) and neo4j (graph database).

Specify neo4j url (`NEO4J_HTTP_URL`) with your login:pass if needed.
You may take look at `batiscaph_app:read_config/0` function for better understanding.

Also you need to install babel to use web interface in development mode:

    npm install .

To start server just type `make`.

# Goals

 * Provide shell UI that encourages exploration of remote erlang node. Provide a map of processes and their relationships.
 * Allow users to inspect processes and have tracing automatically enabled.
 * Record whole session to database, for later inspection and sharing. Should be good for referring to bugs in tickets.

# How it supposed to work

Target erlang have no prerequisites, or have small client library that allows batiscaph to connect to it.

After connection batiscaph uploads client modules to remote node, starts tracer and shell.

Batiscaph collects events from remote node (including shell IO), stores it to database, and displays gathered information in browser.

Batiscaph collects not only traced events (`spawn`, `link`, `exit`) but also calls that may tell more info about explored system. For example `erlang:whereis/1` will add info about registered name to pid, even if this process was not traced and we had no information about it. Same for `erlang:process_info/1,2`. Therefore user can explore system using familiar calls and have returned information recorded and displayed on the map.

Collected events produce graph (neo4j) that gonna be used for querying.

Messages between processes are stored if there not too many of them, and they aren't that big. Need to create throttling mechanism to ignore some of the messages (but still count them).

Having many processes and events recorded, we should be able to query information. If we need to search for a process, not an event, then best way to do it is to construct a pattern of processes and their relationships. A pattern-matching query interface.

It might be useful to have several queries in one workspace, and having interlapsing results highlighted.

# Process tracing levels

 1. Process was mentioned by some other process. No tracing is enabled. Some mentions (but not every) are followed by `erlang:is_process_alive/1` call. If it is alive, it displayed as gray box (according to timestamp) on timeline. If it's dead -- displayed as X. One process may have several mentions connected on timeline.

 1. If mentioned process was selected by user, it turns into "explored" state. After that `exit`, `spawn`, `link` and `unlink` events are traced for this process. Same tracing applies to all process links recursively if process does not have trap_exit enabled. Such strategy will expand linked "islands" of processes, stopping at trap_exit-ed processes (usually supervisors, but also others). Also during exploration some other information gathered, if this process responds to OTP conventions, for example `$ancestors`.
    
    It's important to remember which process was selected by user. If linked process unlinks from that user-selected process, then tracing for it should be stopped. This policy is needed to avoid tracing being trasmitted to too many processes, outside of "linked island" that we were interested in.

1. Process was explicitly enabled to listen additional info. Currently it means collecting messages that were sent by this process. If number of message events exceeds some limit (process-wise and node-wise per second), then one summary event is generated instead. Also it might be great to trace work with ets tables, at least to count updates, inserts and deletions and to which table.

# Other tracing

Usually very few ports are used in apps. Probably it's safe to trace them all for `closed`, `open`, `link` and `unlink`, just like "explored" state for processes. But without having it recursively applied to linked processes. Just mention linked processes.

Probably it's also safe to trace all `ets:new/2`, `ets:give_away/3` and `ets:setopts/2` globally. Collect information about ets tables just like about processes.

If there would be a way to trace all `register`/`unregister` events, without tracing all processes in the system -- would love to have that.

# Shell commands and map effects

 * `[whereis(A) || A <- registered()].`. Will mention and put on the map all processes that were registered on the node.

# Other technical details

 * Clickhouse team recommends to store timestamps with more than second precision in two columns: https://github.com/yandex/ClickHouse/issues/525#issuecomment-282576661, that's why we have at_s and at_mcs. But it's much easier to operate with one column outside of Clickhouse. Neo4j and JavaScript limit integers to be < 2^53, and full microsecond timestamp fits in it. So one `at` field is used outside of Clickhouse.
