-module(room_server).
-behaviour(gen_server).

-export([exists/1, occupants/1, enter/2, leave/2, exits/1, describe/1, traverse/4]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(room, {name, description, exits}).
-record(exit, {name, shortname, description, destination}).

exists(Room) ->
    case db:lookup(mem, rooms, Room) of
        [{Room}] -> true;
        _ -> false
    end.

occupants(Room) ->
    case db:lookup(mem, room_occupants, Room) of
        [{Room, Occupants}] -> {ok, Occupants};
        _ -> []
    end.

exits(Room) ->
    case db:lookup(mem, rooms, Room) of
        [#room{exits=Exits}] -> {ok, Exits};
        _ -> []
    end.


describe(Room) ->
    case db:lookup(rooms, Room) of
        [#room{description=Desc}] -> {ok, Desc};
        _ -> notfound
    end.

enter(Character, Room) ->
    case db:lookup(room_occupants, Room) of
        [] -> db:insert(room_occupants, {Room, [Character]});
        [{Room, Occs}] -> db:insert(room_occupants, {Room, [Character|Occs]})
    end.

leave(Character, Room) ->
    case db:lookup(room_occupants, Room) of
        [Character] -> db:insert(room_occupants, {Room, []});
        [{Room, Occs}] -> db:insert(room_occupants, {Room, lists:delete(Character, Occs)})
    end.

traverse(Character, Pid, CurrentRoom, ExitShortname) ->
    io:format("Character is currently in ~p using exit ~p~n", [CurrentRoom, ExitShortname]),
    leave(Character, CurrentRoom),
    [{_, _, _, _, NewRoom}] = db:lookup(exits_by_shortname, ExitShortname),
    io:format("Moving character to ~p~n", [NewRoom]),
    character_server:move_to(Pid, Character, NewRoom).

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, Path} = application:get_env(sunrise, data_path),
    db:start(both, rooms, [public, named_table, {keypos, 2}], Path),
    ExitToNowhere = #exit{name=tonowhere, shortname=tonowhere, description="To more nowhere", destination=nowhere},
    ExitToMoreNowhere = #exit{name=morenowhere, shortname=morenowhere, description="To even more nowhere", destination=morenowhere},
    NowhereRoom = #room{name=nowhere, description="Literally nowhere.", exits=[ExitToMoreNowhere]},
    MoreNowhereRoom = #room{name=morenowhere, description="Literally even more nowhere.", exits=[ExitToNowhere]},
    db:insert(rooms, NowhereRoom),
    db:insert(rooms, MoreNowhereRoom),
    db:start(mem, room_occupants, [public, named_table]),
    db:start(mem, exits_by_shortname, [public, named_table, {keypos, 3}]),
    db:insert(exits_by_shortname, ExitToNowhere),
    db:insert(exits_by_shortname, ExitToMoreNowhere),
    {ok, {}}.

handle_cast({enter, Character, Room}, State) ->
    case db:lookup(room_occupants, Room) of
        [] -> db:insert(room_occupants, {Room, [Character]});
        [{Room, Occs}] -> db:insert(room_occupants, {Room, [Character|Occs]})
    end,
    {noreply, State};
handle_cast({exit, Character, Room}, State) ->
    case db:lookup(mem, room_occupants, Room) of
        [Character] -> db:insert(room_occupants, {Room, []});
        [{Room, Occs}] -> db:insert(room_occupants, {Room, lists:delete(Character, Occs)})
    end,
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
