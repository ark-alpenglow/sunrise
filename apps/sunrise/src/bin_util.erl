-module(bin_util).
-export([trim/1]).

trim(Bin= <<C, BinTail/binary>>) ->
    case is_whitespace(C) of
        true -> trim(BinTail);
        false -> trim_tail(Bin)
    end.

trim_tail(<<>>) -> <<>>;
trim_tail(Bin) ->
    Size = size(Bin) - 1,
    <<BinHead:Size/binary,C>> = Bin,
    case is_whitespace(C) of
        true -> trim_tail(BinHead);
        false -> Bin
    end.

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.
