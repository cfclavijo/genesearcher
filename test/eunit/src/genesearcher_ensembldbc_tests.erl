-module(genesearcher_ensembldbc_tests).

-ifdef(TEST).

-compile([export_all, nowarn_export_all]).

-include("genesearcher.hrl").
-include("genesearcher_test.hrl").

-include_lib("eunit/include/eunit.hrl").

setup() ->
    meck:new(mysql_poolboy, [passthrough]),
    meck:expect(mysql_poolboy, query,
               fun(?DB_POOL, _Q, [<<"10">>]) -> ?ENSEMBL_GOOD_RESP
               end),
    [mysql_poolboy].

teardown(Mods) ->
    meck:unload(Mods).

all_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
    [{"Test gene suggest", {timeout, 10, fun test_gene_suggest/0}}]}.

test_gene_suggest() ->
    ?assertEqual(?ENSEMBL_GOOD_RESP, genesearcher_ensembldbc:gene_suggest(<<"brc">>, [<<"homo_sapiens">>], <<"10">>)).

prepare_species_param_test() ->
    ?assertEqual(<<"AND species IN ('homo_sapiens','ailuropoda_melanoleuca') ">>, genesearcher_ensembldbc:prepare_species_param([<<"homo_sapiens">>, <<"ailuropoda_melanoleuca">>])).

-endif.
