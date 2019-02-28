%%% @author Carlos Clavijo
%%% @copyright (C) 2019, Carlos Clavijo
%%% @doc
%%% genesearcher Main Module
%%% @end

-module(genesearcher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(routes()),
    HttpPort = genesearcher:get_env(http_port),
    {ok, _} = cowboy:start_clear(
        rest_listener, [{port, HttpPort}], #{env => #{dispatch => Dispatch}}),
    Resp = genesearcher_sup:start_link(),
    io:format("Genesearcher Service Ready", []),
    Resp.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

routes() ->
    HostMatch = '_',
    Paths =
        [{"/gensearcher/v1/gene_suggest", genesearcher_v1_http, [get]}],
    [{HostMatch, Paths}].
