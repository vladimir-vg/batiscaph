All test applications should have a symlink from `_checkouts/vision_probe` to toplevel vision_probe repository.
This is required for prober rebar3 build.

Suites in these repositories take built docker images of test apps and backend,
start them in containers, connect them using erlang internode connection
and check is everything works as expected.