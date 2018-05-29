.PHONY: run ct build_backend build_test_apps
.PHONY: ensure_links_are_in_place

HTTP_PORT = 8999
CLICKHOUSE_URL = http://0.0.0.0:8123/
CLICKHOUSE_TEST_DB = batiscaph_test

run:
	cd backend && make

ct: ensure_links_are_in_place build_backend build_test_apps
	BATISCAPH_ENDPOINT_HTTP_PORT=$(HTTP_PORT) \
	BATISCAPH_ENDPOINT_CLICKHOUSE_URL=$(CLICKHOUSE_URL) \
	BATISCAPH_ENDPOINT_CLICKHOUSE_DB=$(CLICKHOUSE_TEST_DB) \
		./rebar3 ct --name ct_run@0.0.0.0 --setcookie batiscaph-test --suite delta2_SUITE



ensure_links_are_in_place: \
	_checkouts/batiscaph \
	test/erlang17_app1/_checkouts/batiscaph_probe \
	test/phoenix_app1/_checkouts/batiscaph_probe \
	test/erlang17_app1/src/delta_testcases.erl

_checkouts/batiscaph:
	mkdir -p _checkouts
	- cd _checkouts && ln -s ../backend batiscaph

test/erlang17_app1/_checkouts/batiscaph_probe:
	mkdir -p test/erlang17_app1/_checkouts
	cd test/erlang17_app1/_checkouts && ln -s ../../../probe batiscaph_probe

test/phoenix_app1/_checkouts/batiscaph_probe:
	mkdir -p test/phoenix_app1/_checkouts
	cd test/phoenix_app1/_checkouts && ln -s ../../../probe batiscaph_probe

test/erlang17_app1/src/delta_testcases.erl:
	cd test/erlang17_app1/src && ln -s ../../delta_testcases.erl delta_testcases.erl



build_backend:
	cd backend && ./rebar3 compile
	@printf "\n\n"



build_test_apps:
	# docker do not allow to add symlinked files into build
	# but batiscaph_probe has to be symlinked, to run tests on local version
	# do a clever hack: pack app into an archive automatically dereferencing symlinks
	# and feed it into docker build as a context.
	# Found solution here: https://superuser.com/a/842705
	#
	# exclude git repo files and locally built files
	tar --create --dereference \
		--exclude=_checkouts/batiscaph_probe/_build \
		--exclude=_checkouts/batiscaph_probe/.* \
		--exclude=_checkouts/batiscaph_probe/ebin \
		--directory=test/erlang17_app1 . | docker build -t batiscaph-test/erlang17_app1:latest -

	# comment out build of phoenix test app
	# 
	# tar --create --dereference \
	# 	--exclude=_checkouts/batiscaph_probe/_build \
	# 	--exclude=_checkouts/batiscaph_probe/.* \
	# 	--exclude=_checkouts/batiscaph_probe/ebin \
	# 	--directory=test/phoenix_app1 . | docker build -t batiscaph-test/phoenix_app1:latest -
	@printf "\n\n"
