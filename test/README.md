All test applications should have a symlink from `_checkouts/batiscaph_probe` to toplevel probe repository dir.
This is required for prober rebar3 build.

Suites in these repositories take built docker images of test apps and backend,
start them in containers, connect them using erlang internode connection
and check is everything works as expected.

Web and endpoint containers are intentionally left working after test run,
to be able to examine test results via web UI.