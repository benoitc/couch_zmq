%%% -*- erlang -*-
%%%
%%% This file is part of couch_zmq released under the MIT license. 
%%% See the NOTICE for more information.

-module(couch_zmq_pubsub).
-behaviour(gen_server).

-include("couch_db.hrl").

-export([start_link/0, config_change/2]).
-export([send/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop/1]).

-record(state, {
    socket,
    notify_pid,
    dbs = dict:new()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Pid, Change, DbName) ->
    gen_server:cast(Pid, {change, Change, DbName}).

stop(Pid) ->
    couch_db_update_notifier:stop(Pid),
    exit(Pid, shutdown).

init([]) ->
    process_flag(trap_exit, true),
    io:format("init", []),
    ok = couch_config:register(fun couch_zmq_db_updates:config_change/2),
    {ok, _Pid} = zmq:start_link(),
    Uri = couch_config:get("couch_zmq", "pub_spec",
        "tcp://127.0.0.1:7984"),
    case zmq:socket(pub, []) of
        {ok, Socket} ->
             zmq:bind(Socket, Uri),
             Self = self(),
             {ok, Notify} = couch_db_update_notifier:start_link(
                 fun({Change, DbName}) ->
                         ?MODULE:send(Self, Change, DbName);
                     (_) ->
                         ok
                 end),
             {ok, #state{socket=Socket, notify_pid=Notify}};
        Error ->
             ?LOG_ERROR("~p error creating socket: ~p\n", [self(),
                     Error]),
             throw({error, Error})
    end.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({change, updated, DbName}, #state{socket=S, dbs=Dbs}=State) ->
    Options = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
    Dbs1 = case couch_db:open(DbName, Options) of
        {ok, Db} ->
            StartSeq = case dict:find(DbName, Dbs) of
                {ok, Seq} ->
                    Seq;
                error ->
                    couch_db:get_update_seq(Db) - 1
            end,
            {ok, {_, _, LastSeq}} = couch_db:changes_since(Db, main_only, 
                StartSeq, fun handle_changes/2, [{dir, fwd}], 
                {Db, S, StartSeq}),
            couch_db:close(Db),
            dict:store(DbName, LastSeq, Dbs);
        _ ->
            Dbs
    end,
    {noreply, State#state{dbs=Dbs1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    io:format("msg ~p", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reson, #state{notify_pid=Pid, socket=S}) ->
    couch_db_update_notifier:stop(Pid),
    zmq:close(S),
    ok.


handle_changes(DocInfo, {Db, Socket, _}) ->
    #doc_info{id=Id, high_seq=Seq, 
        revs=[#rev_info{deleted=Del,rev=Rev}|_]} = DocInfo,
    ?LOG_INFO("doc info ~p", [Id]),
    Results = [{[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}],
    Row = changes_row(Db, Seq, Id, Del, Results, Rev, false),
    JsonRow = ?JSON_ENCODE(Row),
    DbName = Db#db.name,
    Data = iolist_to_binary([DbName, " ", JsonRow]),
    io:format("send change ~p", [Data]),
    ok = zmq:send(Socket, Data),
    {ok, {Db, Socket, Seq}}.

changes_row(Db, Seq, Id, Del, Results, Rev, true) ->
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del) ++ couch_httpd_view:doc_member(Db, {Id, Rev})};
changes_row(_, Seq, Id, Del, Results, _, false) ->
    {[{<<"seq">>, Seq}, {<<"id">>, Id}, {<<"changes">>, Results}] ++
        deleted_item(Del)}.

deleted_item(true) -> [{<<"deleted">>, true}];
deleted_item(_) -> [].

config_change("couch_zmq", "pub_spec") ->
    [Pid] = [P || {couch_zmq_pubsub,P,_,_}
        <- supervisor:which_children(couch_secondary_services)],
    stop(Pid).
