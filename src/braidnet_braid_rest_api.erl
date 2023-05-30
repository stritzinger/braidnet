-module(braidnet_braid_rest_api).


% API
-export([init/2]).
-export([allowed_methods/2]).
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

resource_exists(#{path := Path} = Req, _State) ->
    Method = filename:basename(Path),
    case Method of
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
	{json_encode(Result), Req, S}.

from_json(Req, <<"launch">> = S) ->
	{ok, Body, Req1} = cowboy_req:read_body(Req),
    LaunchConfig = json_decode(Body),
    Result = braidnet:launch_configuration(LaunchConfig),
    Req2 = cowboy_req:set_resp_body(json_encode(Result), Req1),
	{true, Req2, S}.

json_decode(Msg) -> jsx:decode(Msg, [{return_maps, true}, {labels, binary}]).

json_encode(Msg) -> jsx:encode(Msg).
