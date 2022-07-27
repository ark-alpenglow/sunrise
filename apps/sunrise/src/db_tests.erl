-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

%% ets/dets are builtins, so this module should be kept as minimal
%% as possible and used as an abstraction for mocking.
