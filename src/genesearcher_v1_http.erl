-module(genesearcher_v1_http).
-behavior(cowboy_rest).

%% API
-export([init/2, to_json/2]).

%% REST API
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).

init(Req0, State) ->
    {cowboy_rest, Req0, State}.

%%%===================================================================
%%% REST API
%%%===================================================================

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    io:format("to_accepted~n", []),
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_provided(Req, State) ->
    io:format("to_provided~n", []),
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

to_json(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            case jsonx:decode(Body, [{format, proplist}]) of
                {error, invalid_json, _Pos} ->
                    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, [], Req1);
                PropBody -> %% Happy Path
                    Query = kvc:path(<<"query">>, PropBody),
                    Species = kvc:path(<<"species">>, PropBody),
                    Limit = case kvc:path(<<"limit">>, PropBody) of
                                [] -> genesearcher:get_env(gene_suggest_limit, <<"10">>);
                                InLimit -> InLimit
                            end,
                    io:format("Incoming Values Query:~p Species:~p Limit:~p~n", [Query, Species, Limit]),
                    Suggestions = genesearcher_api:gene_suggest(Query, Species, Limit),
                    Response = jsonx:encode(Suggestions),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1)
            end;
        _ ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, [], Req)
    end.
