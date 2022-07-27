-module(stats_server_tests).
-include_lib("eunit/include/eunit.hrl").

initialize_coins_test() ->
    meck:expect(db, insert, fun
        (stats, {stat, {statid, coins, <<"charname">>}, 1000, 0}) ->    ok
    end),
    stats_server:initialize(<<"charname">>),
    ?assertEqual(1, meck:num_calls(db, insert, '_')),
    meck:validate(db),
    meck:unload(db).

stat_test() ->
    meck:expect(db, lookup, fun
        (stats, {statid, charisma, <<"charname">>}) ->
            ok
    end),
    ?assertEqual(ok, stats_server:stat(<<"charname">>, charisma)),
    meck:validate(db),
    meck:unload(db).
