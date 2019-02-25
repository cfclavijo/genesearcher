-module(genesearcher_api).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("genesearcher.hrl").

-export([gene_suggest/3]).


gene_suggest(_Query, _Species, _Limit) ->
    [].
