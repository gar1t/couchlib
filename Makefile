include ./make/project.mk

shell: apps
	cd lib/couchlib && make shell

test: apps
	cd lib/couchlib; make TESTS=couchlib_tests test
