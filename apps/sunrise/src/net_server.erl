-module(net_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    % Start accepting requests here
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_cast(accept, State=#state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    net_sup:start_socket(),
    ets:insert(connections, {self(), AcceptSocket}),
    io:format("[~p] Connected on pid ~p~n", [AcceptSocket, self()]),
    send(AcceptSocket, "Welcome to sunrise v0.1", []),
    {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
    io:format("Received unknown message.~n"),
    {noreply, State}.

handle_info({tcp, Socket, "quit"++_}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Sender, Msg}, State) ->
    CleanMsg = bin_util:trim(Msg),
    process_message(CleanMsg, Sender, State),
    {noreply, State};
handle_info({send_from_server, Msg}, State=#state{socket=Socket}) ->
    send(Socket, Msg, []),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    ets:delete(connections, self()),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    ets:delete(connections, self()),
    {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

send(Socket, Str) ->
    send(Socket, Str, []).
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    %ok = inet:setopts(Socket, [{active, once}]),
    ok.


process_message(<<"use", " ", Name/binary>>, Sender, _State) ->
    case user_server:name_by_pid(self()) of
        undefined -> send(Sender, "You must log in first.", []);
        {ok, User} ->
            case character_server:enter(self(), User, Name) of
                true -> send(Sender, "Entering world.", []);
                _Res -> 
                    send(Sender, "Couldn't find character.")
            end
    end;
process_message(<<"create", " ", Name/binary>>, Sender, _State) ->
    case user_server:name_by_pid(self()) of
        undefined -> send(Sender, "You must be logged in to create a character.", []);
        {ok, User} ->
            case character_server:create(self(), User, Name) of
                true -> send(Sender, "Created user. Entering world.", []);
                _Res -> 
                    send(Sender, "Couldn't create character.")
            end
    end;
process_message(<<"login", " ", UserAndPass/binary>>, Sender, _State) ->
    [User|Password] = binary:split(UserAndPass, <<" ">>),
    case user_server:login(User, hd(Password)) of
        baduser ->send(Sender, "Invalid username.", []);
        badpass -> send(Sender, "Bad password.", []);
        ok -> 
            send(Sender, "Logged in.", []),
            send(Sender, "Enter the world with `use <character>`", []),
            send(Sender, "Characters available: []", [])
    end;
process_message(<<"register", " ", UserAndPass/binary>>, Sender, _State) ->
    [User|Password] = binary:split(UserAndPass, <<" ">>),
    case user_server:register_user(Sender, User, hd(Password)) of
        userexists -> send(Sender, "User already exists.", []); 
        ok -> 
            send(Sender, "Registered successsfully.", []),
            send(Sender, "Create a character with `create <name>`", [])
    end;
process_message(<<"broadcast", " ", Msg/binary>>, Sender, _State) ->
    io:format("[~p Broadcast] ~s", [Sender, Msg]),
    net_sup:send_all({message, self(), io_lib:format("~p broadcast: ~s", [Sender, Msg])});
process_message(<<"who">>, Sender, _State=#state{socket=Socket}) ->
    io:format("[~p who]~n", [Sender]),
    Users = user_server:connected_users(),
    Msg = string:join([io_lib:format("Name: ~p", [U]) || {_, U} <- Users], "~n"),
    send(Socket, Msg, []);
process_message(Msg, Sender, _State) ->
    send(Sender, "Unknown command: ~p", [Msg]).

