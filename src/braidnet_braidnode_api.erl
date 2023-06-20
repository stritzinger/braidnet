-module(braidnet_braidnode_api).


% API
-export([notify/2]).
-export([notify/3]).

-export([request/3]).
-export([request/4]).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    cid,
    pending_requests = #{}% outgoing requests to braidnode
}).



notify(Pid, Method) ->
    notify(Pid, Method, undefined).

notify(Pid, Method, Params) ->
    Pid ! {notify, Method, Params}.

request(Pid, Caller, Method) ->
    request(Pid, Caller, Method, undefined).

request(Pid, Caller, Method, Params) ->
    Pid ! {request, Caller, Method, Params},
    receive {Pid, Msg} -> Caller ! Msg end.


%--- WS Callbacks --------------------------------------------------------------
init(Req, State) ->
    ?LOG_DEBUG("WS connection attempt..."),
    case cowboy_req:header(<<"id">>, Req, undefined) of
        undefined ->
            ?LOG_ERROR("No container ID"),
            Req1 = cowboy_req:reply(401, Req),
            {ok, Req1, State};
        ID ->
            case braidnet_orchestrator:verify(ID) of
                ok ->
                    {cowboy_websocket, Req, #state{cid = ID}};
                {error, E} ->
                    ?LOG_ERROR("Container ~p failed connecting: ~p",[ID, E]),
                    Req1 = cowboy_req:reply(401, Req),
                    {ok, Req1, State}
            end
    end.

websocket_init(#state{cid = CID} = S) ->
    braidnet_orchestrator:connect(CID, self()),
    {[], S}.

websocket_handle(Frame = {binary, Binary}, #state{cid = CID} = State) ->
    % TODO Refactor using proper jsonRPC handling
    try
        Map = jiffy:decode(Binary, [return_maps]),
        case maps:is_key(<<"jsonrpc">>, Map) of
            true ->
                case maps:is_key(<<"id">>, Map) of
                    true ->
                        {Res, Err} = {maps:is_key(<<"result">>, Map),
                                      maps:is_key(<<"error">>, Map)},
                        case {Res, Err} of
                            {false, false} -> % this is a request
                                {[{binary, handle_request(CID, Map)}], State};
                            {_, _} -> % this is a responce
                                {ok, handle_responce(Map, State)}
                        end;
                    false -> % notification
                        handle_request(CID, Map),
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

websocket_info({notify, Method, Params}, State) ->
    JsonRPC = jsonrpc_object(notification, Method, Params),
    {[{binary, JsonRPC}], State};
websocket_info({request, Caller, Method, Params}, #state{pending_requests = Map} = S) ->
    {ID, JsonRPC} = jsonrpc_object(request, Method, Params),
    {[{binary, JsonRPC}], S#state{pending_requests = Map#{ID => Caller}}};
websocket_info(Info, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Info]),
    {ok, State}.

terminate(Reason, _, #state{cid = CID}) ->
    ?LOG_DEBUG("WS terminated ~p",[Reason]),
    braidnet_orchestrator:disconnect(CID).

handle_request(CID, #{<<"method">> := Method,
                    <<"id">> := Id,
                    <<"params">> := Params}) ->
    Response = case Method of
        <<"register_node">> ->
            #{<<"name">> := Name, <<"port">> := Port} = Params,
            braidnet_epmd_server:register_node(Name, Port);
        <<"address_please">> ->
            #{<<"name">> := Name, <<"host">> := Host} = Params,
            braidnet_epmd_server:address_please(Name, Host);
        <<"names">> ->
            #{<<"node">> := Node, <<"host">> := Host} = Params,
            braidnet_epmd_server:names(Node, Host);
        <<"sign">> ->
            #{<<"payload">> := Payload,
              <<"hash_alg">> := HashAlg,
              <<"sign_alg">> := SignAlg} = Params,
            braidnet_orchestrator:sign(CID, Payload, HashAlg, SignAlg)
    end,
    jsonrpc_response_object(Id, Response);
handle_request(_CID, #{<<"method">> := Method, <<"params">> := _Params}) ->
    case Method of
        _ -> unhandled
    end.

-spec jsonrpc_response_object(binary(), term()) -> jiffy:json_value().
jsonrpc_response_object(Id, Result) when is_tuple(Result) ->
    jsonrpc_response_object(Id, erlang:tuple_to_list(Result));
jsonrpc_response_object(Id, Result) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    jiffy:encode(Map).

jsonrpc_object(Type, Method, Params) ->
    Map1 = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    },
    Map2 = case Params of
        undefined -> Map1;
        _ -> maps:put(<<"params">>, Params, Map1)
    end,
     case Type of
        request ->
            ID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
            {ID,  jiffy:encode(maps:put(<<"id">>, ID, Map2))};
        notification ->
            jiffy:encode(Map2)
    end.

handle_responce(#{<<"id">> := ID} = Map,
                #state{pending_requests = Preqs} = S) ->
    #{ID := Caller} = Preqs,
    Msg = case Map of
        #{<<"error">> := E} -> E;
        #{<<"result">> := R} -> R
    end,
    Caller ! {self(), Msg},
    S#state{pending_requests = maps:remove(ID, Preqs)}.
