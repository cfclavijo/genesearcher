-module(genesearcher_api_tests).

-ifdef(TEST).

-compile([export_all, nowarn_export_all]).

-include("genesearcher.hrl").
-include("genesearcher_test.hrl").

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(genesearcher_ensembldbc, [passthrough]),
    meck:expect(genesearcher_ensembldbc, gene_suggest,
                fun(<<"brc">>, [<<"homo_sapiens">>, <<"ailuropoda_melanoleuca">>], <<"10">>) ->
                        ?ENSEMBL_GOOD_RESP
                end),
    [genesearcher_ensembldbc].

teardown(Mods) ->
    meck:unload(Mods).

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Test gene suggest", {timeout, 10, fun test_gene_suggest/0}}]}.

test_gene_suggest() ->
    ?assertEqual({ok, [<<"BRCA1">>,<<"BRCA2">>,<<"BRCC3">>,<<"BRCC3P1">>]}, genesearcher_api:gene_suggest(<<"brc">>, [<<"homo_sapiens">>, <<"ailuropoda_melanoleuca">>], <<"10">>)),
    ?assertEqual({ok, [<<"BRCA1">>,<<"BRCA2">>,<<"BRCC3">>,<<"BRCC3P1">>]}, genesearcher_api:gene_suggest([{<<"query">>,<<"brc">>}, {<<"species">>,[<<"homo_sapiens">>, <<"ailuropoda_melanoleuca">>]}, {<<"limit">>,<<"10">>}])),
    ?assertEqual({ok, [<<"BRCA1">>,<<"BRCA2">>,<<"BRCC3">>,<<"BRCC3P1">>]}, genesearcher_api:gene_suggest([{<<"query">>,<<"brc">>}, {<<"species">>,[<<"homo_sapiens">>, <<"ailuropoda_melanoleuca">>]}])).

has_params_test() ->
    ?assertEqual(ok, genesearcher_api:has_params([<<"query">>, <<"species">>], [{<<"query">>, <<"brc">>}, {<<"species">>, [<<"specie1">>, <<"specie2">>]}])),
    ?assertEqual(ok, genesearcher_api:has_params([<<"query">>], [{<<"query">>, <<"brc">>}, {<<"species">>, [<<"specie1">>, <<"specie2">>]}])),
    ?assertEqual({error, [<<"query">>]}, genesearcher_api:has_params([<<"query">>, <<"species">>], [{<<"brc">>}, {<<"species">>, [<<"specie1">>, <<"specie2">>]}])).


-endif.
