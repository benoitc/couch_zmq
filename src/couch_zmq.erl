%%% -*- erlang -*-
%%%
%%% This file is part of couch_zmq released under the MIT license. 
%%% See the NOTICE for more information.

-module(couch_zmq).

-export([version/0]).

version() ->
    {ok, FullVersion} = application:get_key(couch_zmq, vsn),
    FullVersion.
