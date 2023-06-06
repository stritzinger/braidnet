-module(braidnet_braid_rest_api).


% API
-export([init/2]).
-export([allowed_methods/2]).
-export([malformed_request/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([to_json/2]).
-export([from_json/2]).

-include_lib("kernel/include/logger.hrl").


%--- API -----------------------------------------------------------------------
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

is_authorized(Req, State) ->
    SecretToken = application:get_env(braidnet, rest_api_token, undefined),
    try cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, SecretToken} -> {true, Req, State};
        _ -> {{false, <<"Bearer">>}, Req, State}
    catch _:_ ->
        {{false, <<"Bearer">>}, Req, State}
    end.

malformed_request(#{path := Path, qs := Qs} = Req, State) ->
    Method = filename:basename(Path),
    case Method of
        <<"logs">> ->
            case get_qs_entry(<<"cid">>, Qs) of
                undefined -> {true, Req, State};
                _CID -> {false, Req, Method}
            end;
        _ -> {false, Req, Method}
    end.

resource_exists(#{path := Path, qs := Qs} = Req, _State) ->
    Method = filename:basename(Path),
    case Method of
        <<"logs">> ->
            CID = get_qs_entry(<<"cid">>, Qs),
            case braidnet_orchestrator:verify(CID) of
                ok -> {true, Req, Method};
                {error, _} -> {false, Req, Method}
            end;
        <<"list">> -> {true, Req, Method};
        <<"destroy">> -> {true, Req, Method};
        _ -> {false, Req, Method}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

delete_resource(Req, <<"destroy">> = S) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    LaunchConfig = json_decode(Body),
    Result = braidnet:remove_configuration(LaunchConfig),
    Req2 = cowboy_req:set_resp_body(json_encode(Result), Req1),
    {true, Req2, S};
delete_resource(Req, <<"destroy">> = S) ->
    {false, Req, S}.

to_json(Req, <<"list">> = S) ->
    Result = braidnet:list(),
    {json_encode(Result), Req, S};
to_json(#{qs := Qs} = Req, <<"logs">> = S) ->
    CID = get_qs_entry(<<"cid">>, Qs),
    Result = braidnet:logs(CID),
    {json_encode(Result), Req, S}.

from_json(Req, <<"launch">> = S) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    LaunchConfig = json_decode(Body),
    Result = braidnet:launch_configuration(LaunchConfig),
    Req2 = cowboy_req:set_resp_body(json_encode(Result), Req1),
    {true, Req2, S}.

json_decode(Msg) -> jiffy:decode(Msg, [return_maps]).

json_encode(Msg) -> jiffy:encode(Msg).

get_qs_entry(Key, Qs) ->
    proplists:get_value(Key, uri_string:dissect_query(Qs)).
