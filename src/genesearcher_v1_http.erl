%%% @author Carlos Clavijo
%%% @copyright (C) 2019, Carlos Clavijo
%%% @doc
%%% REST Handler. Control the lifecycle of POST Calls
%%% @end

-module(genesearcher_v1_http).
-behavior(cowboy_rest).

%% API
-export([init/2, handle_request_json/2]).

%% REST API
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2]).

init(Req0, State) ->
    io:format("to_init~p~n", [Req0]),
    {cowboy_rest, Req0, State}.


%%%===================================================================
%%% REST API
%%%===================================================================

allowed_methods(Req, State) ->
    io:format("to_allowed_methods~n", []),
    {[<<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    io:format("to_provided~n", []),
    {[
      {<<"application/json">>, handle_request_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    io:format("to_accepted~n", []),
    {[
      {<<"*/*">>, handle_request_json},
      {<<"application/json">>, handle_request_json}
     ], Req, State}.

handle_request_json(Req, _State) ->
    io:format("to_handle~n", []),
    case require_json_body(Req) of
        {Data, NewReq} ->
            case genesearcher_api:gene_suggest(Data) of
                {ok, Suggestions} ->
                    Response = jsonx:encode([{<<"suggestions">>, Suggestions}]),
                    json_response(ok, Response, NewReq);
                {error, {missing_parameter = Reason, Res}} ->
                    Response = jsonx:encode([{error_type, Reason},{message, Res}]),
                    json_response(bad_request, Response, NewReq);
                {error, _} ->
                    json_response(not_found, [], NewReq)
            end;
        {error, _Reason, {Status, Response, NewReq}} ->
            json_response(Status, Response, NewReq)
    end.


%%%===================================================================
%%% Internal Functions
%%%===================================================================

json_response(Status, Response, Req) ->
    cowboy_req:reply(response_status(Status), #{<<"content-type">> => <<"application/json">>}, Response, Req).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

require_json_body(Req) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, NewReq} ->
            case jsonx:decode(Body, [{format, proplist}]) of
                {error, invalid_json = Reason, _Pos} ->
                    Resp = jsonx:encode([{<<"error_type">>, <<"error">>}, {<<"message">>, Reason}]),
                    {error, Reason, {bad_request, Resp, NewReq}};
                 Params ->
                    {Params, NewReq}
            end;
        {more, _Body, NewReq} ->
            Reason = request_entity_too_large,
            {error, Reason, {Reason, [], NewReq}}
    end.

response_status(ok) ->
    200;
response_status(created) ->
    201;
response_status(accepted) ->
    202;
response_status(no_content) ->
    204;
response_status(bad_request) ->
    400;
response_status(unauthorized) ->
    401;
response_status(forbiden) ->
    403;
response_status(not_found) ->
    404;
response_status(request_entity_too_large) ->
    413.
