-module(character_server_tests).
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    % inserts into memory database
    % indexes by pid
    % inserts into disk database
    % initializes character stats
    meck:expect(db, insert, fun
        (save, characters, {<<"name">>, <<"user">>, nowhere}) -> ok;
        (mem, characters_by_pid, {_, <<"name">>, <<"user">>, nowhere}) -> ok
    end),
    meck:expect(db, lookup, fun
        (mem, characters, <<"name">>) -> none
    end),
    meck:expect(stats_server, initialize, fun
        (<<"name">>) -> ok
    end),
    character_server:create(none, <<"user">>, <<"name">>),
    meck:validate(db),
    meck:validate(stats_server),
    meck:reset(db),
    meck:reset(stats_server).

create_already_exists_test() ->
    meck:expect(db, lookup, fun
        (mem, characters, <<"name">>) -> [{<<"name">>, <<"user">>, nowhere}];
        (none, none, none) -> [{<<"name">>, <<"user">>, nowhere}]
    end),
    meck:expect(stats_server, initialize, fun
        (<<"name">>) -> ok
    end),
    character_server:create(none, <<"user">>, <<"name">>),
    ?assertEqual(0, meck:num_calls(db, insert, '_')),
    ?assertEqual(1, meck:num_calls(db, lookup, '_')),
    meck:validate(db),
    meck:reset(db).

%enter_does_nothing_if_character_does_not_exist_test() ->
%    % if doesn't exist returns false
%    meck:expect(character_server, character_exists, fun
%        (_) -> false
%    end),
%    ?assertEqual(false, character_server:enter(none, <<"user">>, <<"name">>)),
%    ?assertEqual(0, meck:num_calls(db, insert, '_')),
%    meck:validate(character_server),
%    meck:reset(character_server),
%    meck:reset(db).

enter_test() ->
    % saves pid to memory db
    % enters starting room
    % returns true
    meck:expect(db, lookup, fun (_, _, _) -> [{<<"name">>, none, none}] end),
    meck:expect(db, insert, fun (_, _) -> false end),
    meck:expect(room_server, enter, fun (_, _) -> false end),
    ?assertEqual(true, character_server:enter(<<"sender">>, <<"user">>, <<"name">>)),

    ?assertEqual(1, meck:num_calls(db, insert, '_')),
    ?assertEqual(1, meck:num_calls(room_server, enter, '_')),
    meck:reset(db),
    meck:reset(room_server).

move_to_test() ->
    % updates in-memory character location
    % enters new room
    meck:expect(db, update, fun(_, _, _, _) -> true end),
    meck:expect(room_server, enter, fun (_, _) -> false end),
    character_server:move_to(character, pid, room),
    ?assertEqual(1, meck:num_calls(db, update, '_')),
    ?assertEqual(1, meck:num_calls(room_server, enter, '_')),
    meck:reset(db),
    meck:reset(room_server).

