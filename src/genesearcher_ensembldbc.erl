-module(genesearcher_ensembldbc).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("genesearcher.hrl").

-export([gene_suggest/3]).

gene_suggest(Query, Species, Limit) ->
    DisplayLabel = <<Query/binary, "%">>,
    SpeciesParam = prepare_species_param(Species),
    SqlQuery = <<"
        SELECT display_label, species
        FROM gene_autocomplete
        WHERE
            display_label LIKE '", DisplayLabel/binary,"' ",
            SpeciesParam/binary,
        "GROUP BY display_label
         LIMIT ?">>,
    mysql_poolboy:query(?DB_POOL, SqlQuery, [Limit]).

prepare_species_param([]) ->
    <<>>;
prepare_species_param(Species) ->
    BinSpecies = genesearcher_util:binary_join(Species, <<"','">>),
    <<"AND species IN ('", BinSpecies/binary, "') ">>.
