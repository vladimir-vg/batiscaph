# batiscaph

![Submersible MIR-2](https://pp.userapi.com/c639530/v639530374/4fcba/8ykO3N012hA.jpg)

Batiscaph is a research device that submerges in ocean and explores unknown space.
That's what this tool does -- submerges into Erlang node and observes how it works.

UI ideas for this project could be found here: https://www.figma.com/file/Jz6rhLiGoFbRZOZnoeTBHIvj/Chertezh

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

 - [ ] capture `(init|end)_per_(testcase|group|suite)` common test callbacks into batiscaph steps.
 - [ ] always display spawned processes to right from parent
 - [ ] display all captured tests in structured manner, some kind of replacement for Common Test html reports
 - [ ] automatically switch to fresh common test run for selected test, if detected. Should be convenient to debug one problem, observe one test and switch automatically between runs.
 - [ ] do capture send/receive events for traced processes. Collapse message events on map if have many message events in a row.
 - [ ] script that will export given list of scenarios into static html/jsx files.
 - [ ] display how long it took to execute each line in batiscaph steps.
 - [ ] when context is selected, highlight all time segments where lines of this context were executed.

 - [ ] figure out how to correctly display one process owning many ports.
 - [ ] generate process mention expressions using record syntax when possible.
 - [ ] mechanism of skipping some of the messages if there are too many of them. Properly display that some messages were skipped.
 - [ ] collect and display info about, creating, deleting, owning and transferring (ets:give_away/3) ETS tables.
 - [x] workaround execution of record expressions in erl_eval (batiscaph_steps).
 - [x] calculate list of all testcases from running SUITE:all/0 through erl_eval, allow to specify 'all' in batiscaph_steps attribute
 - [ ] create link-nodes that connected to all linked processes.
       This required to make it clear which processes going to die, if any of them is dead.
       Unlink, link and trap_exit events should create new link-nodes.
 - [ ] run Erlang/OTP tests using batiscaph_steps.
       Should reveal many bugs, and also good way to observe correctness of display.
 - [x] display expression for mentioned processes in steps.
 - [x] fix bug when only first line of multiline expression is copied when run through steps.
 - [ ] find out how to organize JavaScript code that transforms delta and tree. objtree.js is kinda messy now.

# Global ideas to think about

 * Display information about loaded modules in system.
   Might be useful to quickly check what modules and functions are available on running system.
   Also think about displaying versioning information for modules -- when and which module was reloaded,
   what was the differenence in API between them.

 * It would be great to create a mechanism that allows to record communication of the port, and then reproduce it,
   by creating a new one. How it would look like: you reproduce certain request in browser, see a problem.
   Then you start a bot that tries to repeat this request from time to time, while you debugging your system
   and change code. You don't have to rerun request in browser, just change code, reload it and see in browser.
   This kind of bot should replay not only data that was sent, but also the timing.

   If this mechanism could be generalized and bots made smart and flexible, this might become crucial part of development workflow.

   Some kind of a glitch-library where you assemble all kind of glitches that you ever captured in wild,
   and now able to test against and share with others.

 * Being able to make several queries to batiscaph, and having them displayed together with overlaps.
   Query is actually a subscription, it updates if more info is available.

   Should be good for debugging some part of the system -- you may want to see all processes from supervisor X
   with internal id matching Y.

 * Might be good to provide interface to writing complicated tracing rules.
   Just like in Erlang shell, but simplified and convenient as possible.
   No abstractions on top, just as it is in runtime.

 * Human mind operates much better with relative values. That's why it's often hard to understand is part of a code is slow or fast.
   It would be great if every project had some kind of timing test, where you would have different basic operations measured for current machine.

   Operations like:
    - ets:lookup, ets:insert on tables of different size, different types
    - file read/write from disk (or several available disks with different speed)
    - one MTU-sized round-trip TCP send to remote servers in different countries.
    - request to PostgreSQL, MySQL, Redis on local machine. Requests with different tables, different complexity.
    - any other application specific basic operation that developer would like to add, for better understanding of slow and fast code in his system.

   Later having collected all those timings, it would be great to display any other time measurements in system in terms of those recorded.
   Say, this call lasted like two requests to local PostgreSQL or thousand ets:lookup's. Much easier to understand and reason about.
   
   Also it might be useful to display optimization progress: timings of different versions of the system that already recorded could be compared.

# Other technical details

 * Clickhouse team recommends to store timestamps with more than one second precision in two columns: https://github.com/yandex/ClickHouse/issues/525#issuecomment-282576661, that's why we have at_s and at_mcs. But it's much easier to operate with one column outside of Clickhouse. Neo4j and JavaScript limit integers to be < 2^53, and full microsecond timestamp fits in it. So one `at` field is used outside of Clickhouse.

