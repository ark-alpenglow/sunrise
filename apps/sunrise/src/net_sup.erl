-module(net_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0, all_connections/0, send_all/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    ets:new(connections, [public, named_table]),
    {ok, ListenSocket} = gen_tcp:listen(5000, [binary, {reuseaddr, true}]),
    spawn_link(fun empty_listeners/0),
    {ok, 
        { 
            {simple_one_for_one, 60, 3600},
            [
                {net_server, {net_server, start_link, [ListenSocket]}, temporary, 1000, worker, [net_server]}
            ]
        }
    }.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    NListeners = 200,
    [start_socket() || _ <-lists:seq(1,NListeners)],
    ok.

all_connections() ->
    ets:tab2list(connections).

send_all({message, Sender, Msg}) ->
    FilteredConnections = connections_except(Sender),
    io:format("Sending msg from ~p to ~p~n", [Sender, FilteredConnections]),
    send_all(FilteredConnections, Msg);
send_all(Msg) ->
    Connections = all_connections(),
    send_all(Connections, Msg).

send_all([], _Msg) ->
    ok;
send_all([{Pid, _Port}|Rest], Msg) ->
    Pid ! {send_from_server, Msg}, 
    send_all(Rest, Msg).

connections_except(Sender) ->
    All = all_connections(),
    lists:filter(fun ({Pid, _Port}) -> Sender =/= Pid end, All).
