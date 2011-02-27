apps: link-couch
	cd couch && make
	cd mochiweb && make
	cd couchlib && make

clean:
	rm -f couch
	rm -f mochiweb
	cd couchlib && make clean

# Creates symlinks to couchdb source dirs
link-couch:
	@if [ ! -d "$(COUCH_HOME)" ]; then \
	    echo "set COUCH_HOME to couchdb root dir"; exit 1; fi
	ln -fs $(COUCH_HOME)/src/couchdb couch
	ln -fs $(COUCH_HOME)/src/couchdb couch/ebin
	ln -fs $(COUCH_HOME)/src/mochiweb mochiweb
	ln -fs $(COUCH_HOME)/src/mochiweb mochiweb/ebin
