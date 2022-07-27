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
    db:insert(connections, {self(), AcceptSocket}),
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
    db:delete(connections, self()),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    db:delete(connections, self()),
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
    case user_server:register_user(self(), User, hd(Password)) of
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
    Chars = character_server:characters_in_world(),
    Msg = string:join([io_lib:format("Name: ~s", [C]) || C <- Chars], "~n"),
    send(Socket, Msg, []);
process_message(<<"look">>, Sender, _State=#state{socket=Socket}) ->
    case user_server:name_by_pid(self()) of
        undefined -> send(Sender, "You must log in first.", []);
        {ok, _} ->
            io:format("[~p look]~n", [Sender]),
            Loc = character_server:character_location_by_pid(self()),
            {ok, Desc} = room_server:describe(Loc),
            {ok, Exits} = room_server:exits(Loc),
            ExitDescs = [D || {_, D, _, _, _} <- Exits],
            Msg = io_lib:format("~s~nExits: ~p~n", [Desc, ExitDescs]),
            send(Socket, Msg, [])
    end;
process_message(<<"go", " ", Direction/binary>>, Sender, State=#state{socket=Socket}) ->
    io:format("[~p go]~n", [Sender]),
    Char = character_server:character_by_pid(self()),
    Loc = character_server:character_location_by_pid(self()),
    {ok, Exits} = room_server:exits(Loc),
    case [E || {_, E, _, _, _} <- Exits, Direction==atom_to_binary(E)] of
        [Exit] ->
            room_server:traverse(self(), Char, Loc, Exit),
            process_message(<<"look">>, Sender, State);
        _ -> send(Socket, "Unknown direction", [])
    end;
process_message(<<"stats">>, Sender, #state{socket=Socket}) ->
    io:format("[~p stats]~n", [Sender]),
    Char = character_server:character_by_pid(self()),
    Stats = stats_server:all(Char),
    Stats2 = [io_lib:format("~p: ~p/~p~n", [X, Y, Z]) || [X, Y, Z] <- Stats],
    send(Socket, string:join(Stats2, ""));
process_message(<<"check", " ", Stat/binary>>, _, #state{socket=Socket}) ->
    Char = character_server:character_by_pid(self()),
    Result = character_server:check_stat_easy(Char, binary_to_atom(Stat)),
    send(Socket, io_lib:format("~p~n", [Result]));
process_message(<<"hit", " ", Target/binary>>, _, #state{socket=Socket}) ->
    Char = character_server:character_by_pid(self()),
    Result = character_server:use_skill(Char, hit, Target),
    send(Socket, io_lib:format("~p~n", [Result]));
process_message(<<"do", " ", Skill/binary>>, _, #state{socket=Socket}) ->
    Char = character_server:character_by_pid(self()),
    Result = character_server:use_skill(Char, binary_to_atom(Skill)),
    send(Socket, io_lib:format("~p~n", [Result]));
process_message(<<"useon", " ", SkillAndTarget/binary>>, _, #state{socket=Socket}) ->
    [Skill|[Target]] = binary:split(SkillAndTarget, <<" ">>),
    Char = character_server:character_by_pid(self()),
    Result = character_server:use_skill(Char, binary_to_atom(Skill), Target),
    send(Socket, io_lib:format("~p~n", [Result]));
process_message(Msg, Sender, _State) ->
    send(Sender, "Unknown command: ~p", [Msg]).

