-module(genesearcher_api).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("genesearcher.hrl").

-export([gene_suggest/1, gene_suggest/3]).


%% [{<<"query">>, required}, {<<"species">>, required}, {<<"limit">>, optional}]
-spec gene_suggest(proplists:proplists()) -> {ok, proplists:proplists()} | {error, {missing_parameter}}.
gene_suggest(Params) ->
    case has_params([<<"query">>, <<"species">>], Params) of
        ok ->
            Query = kvc:path(<<"query">>, Params),
            Species = kvc:path(<<"species">>, Params),
            Limit = case kvc:path(<<"limit">>, Params) of
                        [] -> genesearcher:get_env(gene_suggest_limit, <<"10">>);
                        InLimit -> InLimit
                    end,
            gene_suggest(Query, Species, Limit);
        {error, Missing} ->
            io:format("Missing parameters ~p~n", [Missing]),
            {error, {missing_parameter, [Missing]}}
    end.

gene_suggest(Query, Species, Limit) ->
    io:format("Procesing in API ~p~n", [[Query, Species, Limit]]),
    {ok, _, Values} = genesearcher_ensembldbc:gene_suggest(Query, Species, Limit),
    Suggest = [D || [D, _S] <- Values],
    {ok, Suggest}.


has_params(Keys, Data) ->
    InKeys = sets:from_list(proplists:get_keys(Data)),
    MainSet = sets:from_list(Keys),
    case sets:to_list(sets:subtract(MainSet, InKeys)) of
        [] ->
            ok;
        Missing ->
            {error, Missing}
    end.
