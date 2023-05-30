-module(braidnet_braidnode_api).


% API
-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include_lib("kernel/include/logger.hrl").


%--- API -----------------------------------------------------------------------
init(Req, State) ->
    ?LOG_DEBUG("WS connection attempt..."),
    case cowboy_req:header(<<"id">>, Req, undefined) of
        undefined ->
            ?LOG_ERROR("No container ID"),
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State};
        ID ->
            case braidnet_container:connect(ID) of
                ok ->
                    {cowboy_websocket, Req, State};
                {error, E} ->
                    ?LOG_ERROR("Container ~p failed connecting: ~p",[ID, E]),
                    Req1 = cowboy_req:reply(401, Req),
                    {ok, Req1, State}
            end
    end.

websocket_handle(Frame = {binary, Binary}, State) ->
    try
        Map = jiffy:decode(Binary, [return_maps]),
        case maps:is_key(<<"jsonrpc">>, Map) of
            true ->
                Response = handle_jsonrpc(Map),
                {[{binary, Response}], State}
        end
    catch E:R:S ->
        ?LOG_DEBUG("~p~n", [{E,R,S}]),
        {[Frame], State}
    end;
websocket_handle(Frame = {text, Text}, State) ->
    ?LOG_DEBUG("Incoming Text: ~p", [Text]),
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(Info, State) ->
    ?LOG_DEBUG("Incoming info: ~p", [Info]),
    {ok, State}.

handle_jsonrpc(#{<<"method">> := Method, <<"id">> := Id, <<"params">> := Params}) ->
    Response = case Method of
        <<"register_node">> -> braidnet_epmd_server:register_node(Params);
        <<"address_please">> -> braidnet_epmd_server:address_please(Params);
        <<"port_please">> -> braidnet_epmd_server:port_please(Params);
        <<"names">> -> braidnet_epmd_server:names(Params);
        <<"connections">> -> braidnet_epmd_server:connections(Params)
    end,
    jsonrpc_response_object(Id, Response).


-spec jsonrpc_response_object(binary(), term()) -> jiffy:json_value().
jsonrpc_response_object(Id, Result) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    jiffy:encode(Map).
