
run_test: build_endpoint build_webapp build_test_apps
	./rebar3 ct --name ct_run@0.0.0.0 --setcookie vision-test

build_endpoint:
	docker build -t vision/endpoint:latest endpoint
	@printf "\n\n"

build_webapp:
	docker build -t vision/web:latest web
	@printf "\n\n"

# TODO: automatically detect if vision_probe is presented in directory,
# also create all symlinks for applications in test/ directory
build_test_apps:
	# docker do not allow to add symlinked files into build
	# but vision_probe has to be symlinked, to run tests on local version
	# do a clever hack: pack app into an archive automatically dereferencing symlinks
	# and feed it into docker build as a context.
	# Found solution here: https://superuser.com/a/842705
	tar --create --dereference --directory=test/example_app1 . | docker build -t vision-test/example_app1:latest -
	@printf "\n\n"
