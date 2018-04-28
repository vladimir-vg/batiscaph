
run_test: build_endpoint build_test_apps
	./rebar3 ct --name ct_run@0.0.0.0 --setcookie vision-test

build_endpoint:
	docker build -t vision/endpoint:latest endpoint
	@printf "\n\n"



# TODO: automatically detect if batiscaph_probe is presented in directory,
# also create all symlinks for applications in test/ directory
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
		--directory=test/erlang_app1 . | docker build -t vision-test/erlang_app1:latest -
	tar --create --dereference \
		--exclude=_checkouts/batiscaph_probe/_build \
		--exclude=_checkouts/batiscaph_probe/.* \
		--exclude=_checkouts/batiscaph_probe/ebin \
		--directory=test/phoenix_app1 . | docker build -t vision-test/phoenix_app1:latest -
	@printf "\n\n"
