-module(user_server).
-behaviour(gen_server).

-export([register_user/3]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%-record(state, {socket}).

register_user(Sender, Username, Password) ->
    CleanUsername = bin_util:trim(Username),
    CleanPassword = bin_util:trim(Password),
    io:format("[~p register] name=~p password=~p~n", [Sender, CleanUsername, CleanPassword]), 
    ets:insert(users_by_name, {CleanUsername, Sender, CleanPassword}),
    ets:insert(users_by_pid, {Sender, CleanUsername, CleanPassword}). 

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    ets:new(users_by_name, [public, named_table]),
    ets:new(users_by_pid, [public, named_table]),
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
