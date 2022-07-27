-module(stats_server).
-behaviour(gen_server).

-export([initialize/1, stat/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(stat, {name, max, current}).
-record(stats, {character, statlist}).

initialize(Name) ->
    db:insert(stats, #stats{character=Name, statlist=[]}).

stat(Character, Stat) ->
    case db:lookup(stats, #stats{character=Character}) of
        #stats{statlist=Statlist} -> Statlist; % todo
        _ -> []
    end.

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, stats, [public, named_table, {keypos, 2}], Path),
    {ok, {}}.

handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.
