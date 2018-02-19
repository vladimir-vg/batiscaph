
run_test: build_test_apps

# TODO: automatically detect if vision_probe is presented in directory,
# also create all symlinks for applications in test/ directory
build_test_apps:
	# docker do not allow to add symlinked files into build
	# but vision_probe has to be symlinked, to run tests on local version
	# doing clever hack: pack app into an archive automatically dereferencing symlinks
	# and feed it into docker build as a context.
	# Found solution here: https://superuser.com/a/842705
	tar --create --dereference --directory=test/example_app1 . | docker build -t vision-test/example_app1:latest -
