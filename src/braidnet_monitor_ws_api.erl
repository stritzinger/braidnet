-module(braidnet_monitor_ws_api).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    pg:join(monitor_client, self()),
    Message = #{
        request => undefined,
        data => <<"Braidnet welcomes you!">>
    },
    {[{text, jiffy:encode(Message)}], State}.

websocket_handle({text, <<"instances">>}, State) ->
    Response = jiffy:encode(#{
        request => instances,
        data => braidnet:instances()
    }),
    {[{text, Response}], State};
websocket_handle(Frame, State) ->
    io:format("Received frame: ~n~p~n", [Frame]),
    {[{text, <<"Hello!">>}], State}.

websocket_info({Type, Data}, State) ->
    BinaryData = erlang:list_to_binary(Data),
    Json = jiffy:encode(#{Type => BinaryData}),
    {[{text, Json}], State}.
