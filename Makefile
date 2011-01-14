# This file is part of couch_zmq released under the MIT license. 
# See the NOTICE for more information.

COUCHDB_SRC ?= "../couchdb/src/couchdb"

.PHONY: deps couchdb_src

all: deps couchdb_src
	./rebar compile

couchdb_src:
	@cp rebar.config rebar.config.orig && \
		cat rebar.config | \
		sed -e "s:../couchdb/src/couchdb:${COUCHDB_SRC}:g" > \
		.~rebar.config && mv .~rebar.config rebar.config

deps: 
	@./rebar get-deps

clean:
	@if [ -f "rebar.config.orig" ]; then \
		mv rebar.config.orig rebar.config; \
	fi
	@./rebar clean

