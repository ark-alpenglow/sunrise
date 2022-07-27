-module(character_server).
-behaviour(gen_server).

-export([create/3, character_exists/1, enter/3, bind_character/2, characters_in_world/0, character_by_pid/1, character_location/1, character_location_by_pid/1, move_to/3]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

create(Sender, User, Name) ->
    case character_exists(Name) of
        true ->
            io:format("Attempted to recreate existing character.~n");
        false ->
            io:format("Creating character ~s for user ~s on pid ~p~n", [Name, User, Sender]),
            db:insert(save, characters, {Name, User, nowhere}),
            db:insert(mem, characters_by_pid, {Sender, Name, User, nowhere}),
            stats_server:initialize(Name)
    end.

enter(Sender, User, Name) ->
    case character_exists(Name) of
        true ->
            Location = character_location(Name),
            io:format("[~p] enter world with ~p at ~p~n", [Sender, Name, Location]),
            db:insert(characters_by_pid, {Sender, Name, User, Location}),
            room_server:enter(Name, Location),
            true;
        false ->
            io:format("[~p,~p] failed to enter world~n", [Name, Sender]),
            false
    end.

character_exists(Name) ->
    case db:lookup(mem, characters, Name) of
        [{Name, _, _}] -> true;
        _ -> false
    end.

character_location(Name) ->
    case db:lookup(mem, characters, Name) of
        [{Name, _, Location}] -> Location;
        _ -> notfound
    end.

character_by_pid(Pid) ->
    case db:lookup(characters_by_pid, Pid) of
        [{Pid, Name, _, _}] -> Name;
        _ -> notfound
    end.

character_location_by_pid(Pid) ->
    case db:lookup(characters_by_pid, Pid) of
        [{Pid, _, _, Location}] -> Location;
        _ -> notfound
    end.

move_to(Character, Pid, Room) ->
    io:format("Moving ~p with pid ~p to ~p~n", [Character, Pid, Room]),
    true = db:update(mem, characters_by_pid, Pid, [{4, Room}]),
    room_server:enter(Character, Room).
    

bind_character(Name, Location) ->
    ok = gen_server:cast(?MODULE, {set_bind, Name, Location}).

characters_in_world() ->
    case db:match(characters_by_pid, '$1') of
        [Results] -> charnames(Results);
        _ -> []
    end.

charnames(CharsByPid) ->
    charnames(CharsByPid, []).
charnames([], Charnames) ->
    Charnames;
charnames([{_, Name, _, _}|T], Charnames) ->
    charnames(T, [Name|Charnames]).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, characters, [public, named_table], Path),
    db:start(mem, characters_by_pid, [public, named_table]),
    {ok, {}}.

handle_cast({set_bind, Name, Location}, State) ->
    % Upgrade old records
    case db:lookup(mem, characters, Name) of
        [{Name, User}] -> 
            db:insert(save, characters, {Name, User, Location});
        [{Name, User, _}] ->
            db:update(save, characters, {Name, User, Location})
    end,
    io:format("Character ~s now has start location of ~s~n", [Name, Location]),
    {noreply, State};
handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.
