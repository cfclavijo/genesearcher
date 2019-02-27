-module(genesearcher_v1_http).
-behavior(cowboy_rest).

%% API
-export([init/2, handle_request_json/2, to_json/2]).

%% REST API
-export([allowed_methods/2, content_types_accepted/2, content_types_provided/2]).

init(Req0, State) ->
    io:format("to_init~n", []),
    {cowboy_rest, Req0, State}.


%%%===================================================================
%%% REST API
%%%===================================================================

allowed_methods(Req, State) ->
    io:format("to_allowed_methods ~p~n", [Req]),
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    io:format("to_accepted~n", []),
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_provided(Req, State) ->
    io:format("to_provided~n", []),
    {[
      {<<"application/json">>, handle_request_json}
     ], Req, State}.

to_json(Req, State) ->
    io:format("to_json~n", []),
    cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, [], Req).

handle_request_json(Req, State) ->
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
                    {ok, Suggestions} = genesearcher_api:gene_suggest(Query, Species, Limit),
                    io:format("Suggestions:~p~n", [Suggestions]),
                    Response = jsonx:encode(Suggestions),
                    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1)
            end;
        _ ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, [], Req)
    end.
