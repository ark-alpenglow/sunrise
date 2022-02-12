-module(character_server).
-behaviour(gen_server).

-export([create/3, character_exists/1, enter/3]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

create(Sender, User, Name) ->
    case character_exists(Name) of
        true ->
            io:format("Attempted to recreate existing character.~n");
        false ->
            io:format("Creating character ~s for user ~s on pid ~p~n", [Name, User, Sender]),
            ets:insert(characters, {Name, User}),
            dets:insert(characters, {Name, User}),
            ets:insert(characters_by_pid, {Sender, Name, User, nowhere})
    end.

enter(Sender, User, Name) ->
    case character_exists(Name) of
        true ->
            io:format("[~p] enter world with ~p~n", [Sender, Name]),
            ets:insert(characters_by_pid, {Sender, Name, User, nowhere});
        false ->
            io:format("[~p,~p] failed to enter world~n", [Name, Sender]),
            ets:insert(characters, {Name, User}),
            dets:insert(characters, {Name, User}),
            ets:insert(characters_by_pid, {Sender, Name, User, nowhere})
    end.

character_exists(Name) ->
    case ets:lookup(characters, Name) of
        [{Name, _}] -> true;
        _ -> false
    end.

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(characters, [public, named_table]),
    ets:new(characters_by_pid, [public, named_table]),
    dets:open_file(characters, [{file, "characters.data"}]),
    dets:to_ets(characters, characters),
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
