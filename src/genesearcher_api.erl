-module(genesearcher_api).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("genesearcher.hrl").

-export([gene_suggest/3]).


gene_suggest(Query, Species, Limit) ->
    io:format("Procesing in API ~p~n", [[Query, Species, Limit]]),
    {ok, _, Values} = genesearcher_ensembldbc:gene_suggest(Query, Species, Limit),
    Suggest = [D || [D, _S] <- Values],
    {ok, Suggest}.
