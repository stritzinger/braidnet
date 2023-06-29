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
    pending_requests = #{} % outgoing requests to braidnode
}).

%--- API -----------------------------------------------------------------------

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

websocket_handle({binary, Binary}, #state{cid = CID} = State) ->
    case braidnet_jsonrpc:decode(Binary) of
        {call, _Method, _Params, _ID} = Call->
            {handle_request(CID, Call), State};
        {notification, _Method, _Params} = Notification ->
            handle_notification(CID, Notification),
            {ok, State};
        {result, _Result, _ID} = Result ->
            {ok, handle_response(Result, State)};
        {error, _Code, _Message, _Data, _ID} = Error ->
            {ok, handle_response(Error, State)};
        {error, _Reason, EncodedReply} ->
            {[{binary, EncodedReply}], State}
    end;
websocket_handle(Frame = {text, Text}, State) ->
    ?LOG_DEBUG("Incoming Text: ~p", [Text]),
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

 websocket_info({notify, Method, undefined}, State) ->
    JsonRPC = braidnet_jsonrpc:notification(Method),
    {[{binary, JsonRPC}], State};
websocket_info({notify, Method, Params}, State) ->
    JsonRPC = braidnet_jsonrpc:notification(Method, Params),
    {[{binary, JsonRPC}], State};
websocket_info({request, Caller, Method, undefined}, #state{pending_requests = Map} = S) ->
    ID = id(),
    JsonRPC = braidnet_jsonrpc:call(Method, ID),
    {[{binary, JsonRPC}], S#state{pending_requests = Map#{ID => Caller}}};
websocket_info({request, Caller, Method, Params}, #state{pending_requests = Map} = S) ->
    ID = id(),
    JsonRPC = braidnet_jsonrpc:call(Method, Params, ID),
    {[{binary, JsonRPC}], S#state{pending_requests = Map#{ID => Caller}}};
websocket_info(Info, State) ->
    ?LOG_WARNING("Unexpected info: ~p", [Info]),
    {ok, State}.

terminate(Reason, _, #state{cid = CID}) ->
    ?LOG_DEBUG("WS terminated ~p",[Reason]),
    braidnet_orchestrator:disconnect(CID).

% internal ---------------------------------------------------------------------

handle_request(CID, {call, Method,  Params, ID}) ->
    JSON =
    try call_method(Method, Params, CID) of
        undefined ->  braidnet_jsonrpc:error(method_not_found, ID);
        Result -> braidnet_jsonrpc:result(Result, ID)
    catch Ex:Er:Stack ->
        ?LOG_ERROR("JsonRPC internal error ~p : ~p : ~p",[Ex, Er, Stack]),
        braidnet_jsonrpc:error(internal_error, ID)
    end,
    [{binary, JSON}].

handle_notification(_CID, {notification, Method, _Params}) ->
    ?LOG_WARNING("Unhandled jsonrpc notification method ~p",[Method]).

handle_response({result, Result, ID}, #state{pending_requests = Preqs} = S) ->
    #{ID := Caller} = Preqs,
    Caller ! {self(), Result},
    S#state{pending_requests = maps:remove(ID, Preqs)};
handle_response({error, _, _, _, ID} = Error,
                #state{pending_requests = Preqs} = S) ->
    #{ID := Caller} = Preqs,
    Caller ! {self(), Error},
    S#state{pending_requests = maps:remove(ID, Preqs)}.

id() -> uuid:uuid_to_string(uuid:get_v4(), binary_standard).

call_method(<<"register_node">>, #{<<"name">> := Name, <<"port">> := Port},
            _CID) ->
    {ok, Cons} = braidnet_epmd_server:register_node(Name, Port),
    #{connections => Cons};
call_method(<<"address_please">>,#{<<"name">> := Name, <<"host">> := Host},
            _CID) ->
    case braidnet_epmd_server:address_please(Name, Host) of
        {ok, Addr, Port} -> #{address => Addr, port => Port};
        {error, Reason} -> #{error => Reason}
    end;
call_method(<<"names">>, #{<<"node">> := Node, <<"host">> := Host},
            _CID) ->
    case braidnet_epmd_server:names(Node, Host) of
        {ok, Map} -> Map;
        {error, Reason} -> #{error => Reason}
    end;
call_method(<<"sign">>,
            #{<<"payload">> := Payload,
              <<"hash_alg">> := HashAlg,
              <<"sign_alg">> := SignAlg},
            CID) ->
    case braidnet_orchestrator:sign(CID, Payload, HashAlg, SignAlg) of
        Binary -> Binary;
        {error, E} -> #{error => E}
    end;
call_method(_, _, _) ->
    undefined.
