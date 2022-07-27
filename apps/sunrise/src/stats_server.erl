-module(stats_server).
-behaviour(gen_server).

-export([initialize/1, stat/2, stat_value/2, all/1, bodysystems/1, bodysystem_status/2, damage/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(statid, {name, character}).
-record(stat, {id, max, current}).
-record(bodysystem, {id, status, priority}).
%% status is one of [healthy, degraded, impaired, destroyed]
%% priority is one of [core, peripheral]

initialize(Name) ->
    db:insert(save, stats, #stat{id=#statid{name=coins, character=Name}, max=1000, current=0}),
    db:insert(save, stats, #stat{id=#statid{name=strength, character=Name}, max=100, current=20}),
    db:insert(save, stats, #stat{id=#statid{name=dexterity, character=Name}, max=100, current=20}),
    db:insert(save, stats, #stat{id=#statid{name=constitution, character=Name}, max=100, current=20}),
    db:insert(save, stats, #stat{id=#statid{name=intelligence, character=Name}, max=100, current=20}),
    db:insert(save, stats, #stat{id=#statid{name=wisdom, character=Name}, max=100, current=20}),
    db:insert(save, stats, #stat{id=#statid{name=charisma, character=Name}, max=100, current=20}),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=leftarm, character=Name},
            status=healthy,
            priority=peripheral
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=rightarm, character=Name},
            status=healthy,
            priority=peripheral
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=leftleg, character=Name},
            status=healthy,
            priority=peripheral
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=rightleg, character=Name},
            status=healthy,
            priority=peripheral
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=heart, character=Name},
            status=healthy,
            priority=core
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=brain, character=Name},
            status=healthy,
            priority=core
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=mind, character=Name},
            status=healthy,
            priority=core
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=leftlung, character=Name},
            status=healthy,
            priority=core
    }),
    db:insert(save, bodysystems,
        #bodysystem{
            id=#statid{name=rightlung, character=Name},
            status=healthy,
            priority=core
    }).

stat(Character, Stat) ->
    db:lookup(stats, #statid{name=Stat, character=Character}).

stat_value(Character, Stat) ->
    io:format("Getting value of stat: ~p~n", [Stat]),
    case stat(Character, Stat) of 
        [#stat{current=Val}] -> Val;
        [] -> notfound
    end.

all(Character) ->
    db:match(stats, 
        #stat{
            id=#statid{name='$1', character=Character},
            max='$3',
            current='$2'
    }).

bodysystems(Character) ->
    db:match(bodysystems,
        #bodysystem{
            id=#statid{name='$1', character=Character},
            status='$2',
            priority='$3'
    }).

bodysystem_status(Character, Name) ->
    [Result] = db:match(bodysystems,
        #bodysystem{
            id=#statid{name=Name, character=Character},
            status='$1',
            priority='_'
    }),
    Result.

bodysystem(Character, Name) ->
    [Result] = db:match(bodysystems,
        #bodysystem{
            id=#statid{name=Name, character=Character},
            status='$1',
            priority='$2'
    }),
    Result.

damage(Character, BodySystem) ->
    [CurrentStatus, Priority] = bodysystem(Character, BodySystem),
    NewStatus = case CurrentStatus of
        healthy -> degraded;
        degraded -> impaired;
        impaired -> destroyed;
        destroyed -> destroyed
    end,
    io:format("Damage to ~p ~p results in ~p.~n", [Character, BodySystem, NewStatus]),
    db:update(save, bodysystems, #bodysystem{
        id=#statid{name=BodySystem, character=Character},
        status = NewStatus,
        priority = Priority
    }).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, stats, [public, named_table, {keypos, 2}], Path),
    db:start(both, bodysystems, [public, named_table, {keypos, 2}], Path),
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
