-module(genesearcher_util).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export([binary_join/2]).

binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun (A, B) ->
                        if
                            bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
                            true -> A
                        end
                end, <<>>, List).
