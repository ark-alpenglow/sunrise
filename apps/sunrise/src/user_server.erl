-module(user_server).
-behaviour(gen_server).

-export([register_user/3, validate_user/2, all_users/0, connected_users/0, login/2, is_logged_in/1, name_by_pid/1]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%-record(state, {socket}).

register_user(Sender, Username, Password) ->
    CleanUsername = bin_util:trim(Username),
    CleanPassword = bin_util:trim(Password),
    case validate_user(Username, Password) of
        ok -> userexists;
        badpass -> userexists;
        baduser ->
            io:format("[~p register] name=~p password=~p~n", [Sender, CleanUsername, CleanPassword]), 
            ets:insert(users, {CleanUsername, CleanPassword}),
            dets:insert(users, {CleanUsername, CleanPassword}),
            ets:insert(users_by_pid, {Sender, CleanUsername}),
            login(Username, Password),
            ok
    end.

all_users() ->
    ets:tab2list(users).

connected_users() ->
    ets:tab2list(users_by_pid).

login(Username, Password) ->
    CleanUsername = bin_util:trim(Username),
    CleanPassword = bin_util:trim(Password),
    gen_server:call(?MODULE, {login, CleanUsername, CleanPassword}).

validate_user(Username, Password) ->
    CleanUsername = bin_util:trim(Username),
    CleanPassword = bin_util:trim(Password),
    gen_server:call(?MODULE, {validate, CleanUsername, CleanPassword}).

is_logged_in(Pid) ->
    case ets:lookup(users_by_pid, Pid) of
        [] -> false;
        [{Pid, _}] -> true;
        _ -> unknown
    end.

name_by_pid(Pid) ->
    case ets:lookup(users_by_pid, Pid) of
        [] -> undefined;
        [{Pid, User}] -> {ok, User};
        _ -> undefined
    end.

start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ets:new(users, [public, named_table]),
    ets:new(users_by_pid, [public, named_table]),
    {ok, Path} = application:get_env(sunrise, data_path),
    dets:open_file(users, [{file, io_lib:format("~s/users.data", [Path])}]),
    dets:to_ets(users, users),
    {ok, {}}.

handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call({login, User, Pass}, {From, _}, State) ->
    Result = ets:lookup(users, User),
    case validate_user_pass(Result, User, Pass) of
        ok -> 
            ets:insert(users_by_pid, {From, User}),
            {reply, ok, State};
        baduser -> {reply, baduser, State};
        badpass -> {reply, badpass, State}
    end;
handle_call({validate, User, Pass}, _From, State) ->
    Result = ets:lookup(users, User),
    {reply, validate_user_pass(Result, User, Pass), State};
handle_call(_E, _From, State) -> {noreply, State}.

terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

validate_user_pass([], _, _) -> baduser;
validate_user_pass([{DbUser, DbPass}], User, Pass) when DbUser =:= User, DbPass =:= Pass -> ok;
validate_user_pass(_, _, _) -> badpass.
