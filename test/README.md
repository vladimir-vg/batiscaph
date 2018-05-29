All test applications should have a symlink from `_checkouts/batiscaph_probe` to toplevel probe repository dir.
This is required for prober rebar3 build. These symlinks are automatically created during Makefile test run.

Suites in these repositories take built docker images of test apps,
start them in containers.
