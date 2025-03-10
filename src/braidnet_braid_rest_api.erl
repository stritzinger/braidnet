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

malformed_request(#{bindings := #{method := <<"instances">>}} = Req, S) ->
    {false, Req, S};
malformed_request(#{bindings := #{method := <<"list">>}} = Req, S) ->
    {false, Req, S};
malformed_request(#{bindings := #{method := <<"logs">>}} = Req, S) ->
    case qs_keys_exist([cid], Req) of
        false -> {true, Req, S};
        true -> {false, Req, S}
    end;
malformed_request(#{bindings := #{method := <<"rpc">>}}= Req, S) ->
    Keys = [cid, m, f, args],
    case qs_keys_exist(Keys, Req) of
        false -> {true, Req, S};
        true -> {false, Req, S}
    end;
malformed_request(Req, S) ->
    case check_config(Req) of
        {ok, Cfg} -> {false, Req, Cfg};
        error -> {true, Req, S}
    end.

resource_exists(#{bindings := #{method := M}, qs := Qs} = Req, State)
when M == <<"logs">> orelse M == <<"rpc">> ->
    CID = get_qs_entry(<<"cid">>, Qs),
    case braidnet_orchestrator:verify(CID) of
        ok -> {true, Req, State};
        {error, _} -> {false, Req, State}
    end;
resource_exists(#{bindings := #{method := <<"instances">>}} = Req, State) ->
    {true, Req, State};
resource_exists(#{bindings := #{method := <<"list">>}} = Req, State) ->
    {true, Req, State};
resource_exists(#{bindings := #{method := <<"destroy">>}} = Req, State) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

delete_resource(#{bindings := #{method := <<"destroy">>}} = Req, BraidCfg = S) ->
    braidnet:remove_configuration(BraidCfg),
    {true, Req, S}.

to_json(#{bindings := #{method := <<"instances">>}} = Req, S) ->
    Result = braidnet:instances(),
    {json_encode(Result), Req, S};
to_json(#{bindings := #{method := <<"list">>}} = Req, S) ->
    Result = braidnet:list(),
    {json_encode(Result), Req, S};
to_json(#{bindings := #{method := <<"logs">>}, qs := Qs} = Req, S) ->
    CID = get_qs_entry(<<"cid">>, Qs),
    Result = braidnet:logs(CID),
    {json_encode(Result), Req, S};
to_json(#{bindings := #{method := <<"rpc">>}, qs := Qs} = Req, S) ->
    CID = get_qs_entry(<<"cid">>, Qs),
    M = get_qs_entry(<<"m">>, Qs),
    F = get_qs_entry(<<"f">>, Qs),
    A = get_qs_entry(<<"args">>, Qs),
    case braidnet:rpc(CID, M, F, A) of
        {ok, Result} ->
            {json_encode(Result), Req, S};
        {error, Reason} ->
            ErrorMsg = base64:encode(iolist_to_binary(io_lib:format("~p",[Reason]))),
            Req2 = cowboy_req:reply(400, #{}, ErrorMsg, Req),
            {stop, Req2, S}
    end.

from_json(#{bindings := #{method := <<"launch">>}} = Req, BraidCfg = S) ->
    braidnet:launch_configuration(BraidCfg),
    {true, Req, S}.

% INTERNALS --------------------------------------------------------------------

json_decode(Msg) -> jiffy:decode(Msg, [return_maps]).

json_encode(Msg) -> jiffy:encode(Msg).

qs_keys_exist(Entries, Req) ->
    try
        cowboy_req:match_qs([{K, [nonempty]} || K <- Entries], Req),
        true
    catch _:_ ->
        false
    end.

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
