# This file is part of couch_zmq released under the MIT license. 
# See the NOTICE for more information.

COUCHDB_SRC ?= "../couchdb/src/couchdb"

.PHONY: deps couchdb_src

all: deps couchdb_src
	@./rebar compile 

dev: deps dev_couchdb_src
	@./rebar -C rebar-dev.config compile	

couchdb_src:
	@cat rebar.config.template | \
		sed -e "s:@COUCHDB_SRC@:${COUCHDB_SRC}:g" > \
		rebar.config

dev_couchdb_src:
	@cat rebar.config.template | \
		sed -e "s:@COUCHDB_SRC@:${COUCHDB_SRC}:g" > \
		rebar-dev.config
	
deps: 
	@./rebar get-deps

clean:
	@rm -rf rebar-dev.config
	@./rebar clean

distclean: clean
	@rm -rf deps
