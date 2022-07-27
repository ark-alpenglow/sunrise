-module(bin_util_tests).
-include_lib("eunit/include/eunit.hrl").

trim_test() ->
    <<"2">> = bin_util:trim(<<" 2 ">>),
    <<"1 2 3 4 5">> = bin_util:trim(<<" 1 2 3 4 5 ">>),
    <<"one two three four five">> = bin_util:trim(<<"    one two three four five   ">>). 
