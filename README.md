This project at first should provide convenient visual interface to Erlang shell.

Particularly to illustrate traces for some code examples in [Learn you some Erlang](http://learnyousomeerlang.com/content) and [Programming Erlang](https://pragprog.com/book/jaerlang2/programming-erlang) books.

Actually I've tried to create tool that collects and visualizes events from Common Test runs before: [screen1](/common_test_prev_attempt1.png), [screen2](/common_test_prev_attempt2.png), [screen3](/common_test_prev_attempt3.png). But it was too coupled with proprietary code from my last workplace, so I decided to start it from scratch.

# not forget to implement

- [ ] make pid in shell IO clickable, select processes

# TODO

If this project becomes something bigger than just demo, I would like to have:

 * Events storage (Clickhose?) and process dependency aggregation (some graph DB? neo4j?)
 * UI that allows to make several queries (across several applications) and display composed results on one map, aligned by time.
 * UI that gives overview of process tree in Erlang application, extracted from recorded events.
 * UI that allows to query in pattern-matching like manner over processes and their properties.
 * Be able to connect to Erlang remote shell, upload our tracing code (no preparation on remote side required) and receive selected set of events. Record event streams from such erlang shell sessions, make it searchable.
 * Be able to receive events from Common Test runs, for more detailed report and debug.
 * Support other kinds of events sources, Ruby, Python applications or even events from browser side. Having all it aligned on one timeline should give powerful insight on what's happening in application.