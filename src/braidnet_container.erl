-module(braidnet_container).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_link/0,
         launch/1,
         connect/1,
         event/2]).

-include_lib("kernel/include/logger.hrl").
-include("braidnet_internal.hrl").

-record(state, {
    containers  = #{} :: #{container_id() => #container{}}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

launch(DockerImage) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, DockerImage}).

connect(ContainerID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerID}).

event(ContainerID, Event) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, Event}).

% gen_server callbacks ---------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({connect, ContainerID}, _, #state{containers = CTNs} = S) ->
    case maps:is_key(ContainerID, CTNs) of
        true->
            ?LOG_NOTICE("Container ~p connected to the braidnet.", [ContainerID]),
            {reply, ok, update_container_status(ContainerID, running, S)};
        false -> {reply, {error, unexpected_id}, S}
    end;
handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast({launch, DockerImage},
            #state{containers = Containers} = S) ->
    Cmd = "docker run --rm -d " ++ DockerImage,
    ContainerID = string:trim(os:cmd(Cmd)),
    ShortID = list_to_binary(string:slice(ContainerID, 0, 12)),
    ?LOG_NOTICE("Started container ~p",[ShortID]),
    Container = #container{image = DockerImage, status = unknown},
    {noreply, S#state{containers = Containers#{ShortID => Container}}};
handle_cast({event, ContainerID, Event}, #state{containers = CTNs} = S) ->
    NewS = case maps:is_key(ContainerID, CTNs) of
        true-> handle_event(ContainerID, Event, S);
        false ->
            ?LOG_ERROR("Event from unexpected container ~p", [ContainerID]),
            S
    end,
    {noreply, NewS};
handle_cast(_, S) ->
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected msg: ~p",[Msg]),
    {noreply, S}.


update_container_status(ContainerID, NewStatus, #state{containers = CTNs} = S) ->
    #{ContainerID := CTN} = CTNs,
    Container = CTN#container{status = NewStatus},
    NewMap = maps:put(ContainerID, Container, CTNs),
    S#state{containers = NewMap}.


handle_event(ContainerID, disconnected, #state{containers = CTNs} = S) ->
    ?LOG_NOTICE("Container ~p lost connection", [ContainerID]),
    #{ContainerID := CTN} = CTNs,
    Container = CTN#container{status = lost},
    NewMap = maps:put(ContainerID, Container, CTNs),
    S#state{containers = NewMap}.

