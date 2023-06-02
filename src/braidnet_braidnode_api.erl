-module(braidnet_braidnode_api).


% API
-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {container_id}).

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
                    {cowboy_websocket, Req, #state{container_id = ID}};
                {error, E} ->
                    ?LOG_ERROR("Container ~p failed connecting: ~p",[ID, E]),
                    Req1 = cowboy_req:reply(401, Req),
                    {ok, Req1, State}
            end
    end.

websocket_handle(Frame = {binary, Binary}, #state{container_id = CID} = State) ->
    try
        Map = jiffy:decode(Binary, [return_maps]),
        case maps:is_key(<<"jsonrpc">>, Map) of
            true ->
                case maps:is_key(<<"id">>, Map) of
                    true ->  % reply to request
                        {[{binary, handle_jsonrpc(CID, Map)}], State};
                    false -> % notification
                        handle_jsonrpc(CID, Map),
                        {ok, State}
                end
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

terminate(Reason, _, #state{container_id = ContainerId}) ->
    ?LOG_DEBUG("WS terminated ~p",[Reason]),
    braidnet_container:disconnect(ContainerId).

handle_jsonrpc(_, #{<<"method">> := Method,
                    <<"id">> := Id,
                    <<"params">> := Params}) ->
    Response = case Method of
        <<"register_node">> -> braidnet_epmd_server:register_node(Params);
        <<"address_please">> -> braidnet_epmd_server:address_please(Params);
        <<"names">> -> braidnet_epmd_server:names(Params)
    end,
    jsonrpc_response_object(Id, Response);
handle_jsonrpc(CID, #{<<"method">> := Method, <<"params">> := Params}) ->
    case Method of
        <<"log">> ->
            #{<<"text">> := Text} = Params,
            braidnet_container:log(CID, Text)
    end.


-spec jsonrpc_response_object(binary(), term()) -> jiffy:json_value().
jsonrpc_response_object(Id, Result) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    jiffy:encode(Map).
