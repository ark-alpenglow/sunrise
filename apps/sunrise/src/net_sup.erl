-module(net_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, ListenSocket} = gen_tcp:listen(5000, [{active, once}]),
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
    NListeners = 20,
    [start_socket() || _ <-lists:seq(1,NListeners)],
    ok.
