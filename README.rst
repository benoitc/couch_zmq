About
-----

couch_zmq is a zeromq endpoint for couchdb.

Install
-------

Make sure Couchdb is installed on folder above the couch_zmq one. You
can also change the path of couchdb installation in rebar.config.

::

    $ make

Add couch_zmq_pubsub server to couch configuration file *local_dev.ini* 
and edit the **[daermons]** section::

    [daemons]
    couch_zmq_pubsub={couch_zmq_pubsub, start_link, []}

Start couchdb::

    $ export $COUCH_ZMQ=/path/to/couch_zmq
    $ ERL_FLAGS="-pa $COUCH_ZMQ/ebin -pa $COUCH_ZMQ/deps/zmq/ebin" ./utils/run
    

Now you can suscribe to zeromq on tcp://127.0.0.1:7984 port by default
(you can change the uri in  section couch_zmq by adding db_updates key.


Here is a simple script in Python to listen db changes::

    import zmq
    context = zmq.Context()
    socket = context.socket(zmq.SUB)
    socket.connect("tcp://127.0.0.1:7984")

    socket.setsockopt(zmq.SUBSCRIBE, "testdb")


    while True:
        msg = socket.recv()
        print "Got", msg


Start it::
    
    $ python test.py

Then create document in *testdb* database and listen for it::

    $ python test.py 
    Got testdb {"seq":53,"id":"fa1ad2fc27dd45870698b2c199000277","changes":[{"rev":"53-d6de3b672dfe3ff22d176ddc9f2b2be2"}]}
    Got testdb {"seq":54,"id":"fa1ad2fc27dd45870698b2c199000277","changes":[{"rev":"54-3e896e93ca745d8299d52fed313e8a64"}]}
    Got testdb {"seq":55,"id":"fa1ad2fc27dd45870698b2c199000277","changes":[{"rev":"55-229b77d591deee30dbde967ca288ced1"}]}
    Got testdb {"seq":56,"id":"fa1ad2fc27dd45870698b2c199000277","changes":[{"rev":"56-459ad8f3d6e0e99ba2438c715dba5e64"}]}
    

more soon.


TODO:
-----

- Full CouchDB API access
- Changes notification handling / clients


Changelog:
---------

version 0.01:
+++++++++++++

 - Show the usage of couch_zmq. simple PUB/SUB pattern to sucribe to one
   or all db changes.
