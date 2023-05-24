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
                    ?LOG_ERROR("Container ~p failed with error: ~p",[ID, E]),
                    Req1 = cowboy_req:reply(401, Req),
                    {ok, Req1, State}
            end
    end.

websocket_handle(Frame = {text, Text}, State) ->
    ?LOG_DEBUG("Incoming Text : ~p", [Text]),
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {ok, State}.