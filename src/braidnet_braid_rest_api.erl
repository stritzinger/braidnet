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
        <<"list">> -> {false, Req, State};
        <<"logs">> ->
            case get_qs_entry(<<"cid">>, Qs) of
                undefined -> {true, Req, State};
                _CID -> {false, Req, State}
            end;
        _ ->
            case check_config(Req) of
                {ok, Cfg} -> {false, Req, Cfg};
                error -> {true, Req, State}
            end
    end.

resource_exists(#{path := Path, qs := Qs} = Req, State) ->
    Method = filename:basename(Path),
    case Method of
        <<"logs">> ->
            CID = get_qs_entry(<<"cid">>, Qs),
            case braidnet_orchestrator:verify(CID) of
                ok -> {true, Req, State};
                {error, _} -> {false, Req, State}
            end;
        <<"list">> -> {true, Req, State};
        <<"destroy">> -> {true, Req, State};
        _ -> {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

delete_resource(#{bindings := #{method := <<"destroy">>}} = Req0, BraidCfg = S) ->
    braidnet:remove_configuration(BraidCfg),
    Req1 = cowboy_req:reply(202, Req0),
    {true, Req1, S}.

to_json(#{bindings := #{method := <<"list">>}} = Req, S) ->
    Result = braidnet:list(),
    {json_encode(Result), Req, S};
to_json(#{bindings := #{method := <<"logs">>}, qs := Qs} = Req, S) ->
    CID = get_qs_entry(<<"cid">>, Qs),
    Result = braidnet:logs(CID),
    {json_encode(Result), Req, S}.

from_json(#{bindings := #{method := <<"launch">>}} = Req0, BraidCfg = S) ->
    braidnet:launch_configuration(BraidCfg),
    Req1 = cowboy_req:reply(202, Req0),
    {true, Req1, S}.

% INTERNALS --------------------------------------------------------------------

json_decode(Msg) -> jiffy:decode(Msg, [return_maps]).

json_encode(Msg) -> jiffy:encode(Msg).

get_qs_entry(Key, Qs) ->
    proplists:get_value(Key, uri_string:dissect_query(Qs)).

check_config(Req) ->
    {ok, Body, _Req1} = cowboy_req:read_body(Req),
    try json_decode(Body) of
        Cfg when is_map(Cfg) ->
            {ok, Cfg};
        _ -> error
    catch _:_ -> error
    end.
