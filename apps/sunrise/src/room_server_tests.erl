-module(room_server_tests).
-include_lib("eunit/include/eunit.hrl").

exists_true_test() ->
    meck:expect(db, lookup, fun (_, _, <<"room">>) -> [{<<"room">>}] end),
    ?assertEqual(true, room_server:exists(<<"room">>)),
    ?assertEqual(1, meck:num_calls(db, lookup, '_')),
    meck:validate(db),
    meck:reset(db).

exists_false_test() ->
    meck:expect(db, lookup, fun (_, _, <<"room">>) -> notfound end),
    ?assertEqual(false, room_server:exists(<<"room">>)),
    ?assertEqual(1, meck:num_calls(db, lookup, '_')),
    meck:validate(db),
    meck:reset(db).

occpuants_test() ->
    meck:expect(db, lookup, fun (_, _, <<"room">>) -> [{<<"room">>, [occ1, occ2]}] end),

    ?assertEqual({ok, [occ1, occ2]}, room_server:occupants(<<"room">>)),
    ?assertEqual(1, meck:num_calls(db, lookup, '_')),
    meck:validate(db),
    meck:reset(db).

exits_test() ->
    meck:expect(db, lookup, fun (mem, rooms, <<"room">>) -> 
        [{room, <<"room">>, description, [exit1, exit2]}]
    end),
    ?assertEqual({ok, [exit1, exit2]}, room_server:exits(<<"room">>)),
    ?assertEqual(1, meck:num_calls(db, lookup, '_')),
    meck:validate(db),
    meck:reset(db).
