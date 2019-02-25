-module(genesearcher).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("genesearcher.hrl").

-export([start/0, stop/0]).
-export([get_env/1, get_env/2, set_env/2]).

%%===================================================================
%% API Functions
%%===================================================================

start() ->
    {ok, _} = application:ensure_all_started(?APPLICATION).


stop() ->
    application:stop(?APPLICATION).


get_env(Key) ->
    get_env(Key, 'undefined').


get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, X} -> X;
        'undefined' -> Default
    end.


set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).
