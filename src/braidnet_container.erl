-module(braidnet_container).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% External API
-export([start_link/0,
         launch/2,
         list/0,
         delete/1]).

% Internal API
-export([connect/1,
         disconnect/1,
         log/2,
         event/2]).

-include_lib("kernel/include/logger.hrl").

-type container_id() :: binary().

-record(container, {
    name                :: binary(),
    image               :: binary(),
    status = unknown    :: unknown | running | lost | paused
}).

-record(state, {
    containers  = #{} :: #{container_id() => #container{}}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

launch(Name, Opts) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Name, Opts}).

list() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

delete(ContainerName) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerName}).

connect(ContainerID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerID}).

disconnect(ContainerID) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID}).

log(ContainerID, Text) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, Text}).

event(ContainerID, Event) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, Event}).

% gen_server callbacks ---------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({connect, ContainerID}, _, #state{containers = CTNs} = S) ->
    case CTNs of
        #{ContainerID := #container{name = N}} ->
            ?LOG_NOTICE("Container ~p connected to the braidnet.", [N]),
            {reply, ok, update_container_status(ContainerID, running, S)};
        _ -> {reply, {error, unexpected_id}, S}
    end;

handle_call(list, _, #state{containers = Containers} = S) ->
    List =
    [#{id => ID,
       name => Name,
       image => Image,
       status => Status} ||
        {ID, #container{name = Name, image = Image, status = Status}}
            <- maps:to_list(Containers)],
    {reply, List, S};

handle_call({delete, ContainerName}, _, #state{containers = CTNs} = S) ->
    Partition = lists:partition(
        fun ({_ ,#container{name = Name}}) when Name == ContainerName -> true;
            (_) -> false
        end, maps:to_list(CTNs)),
    Remaining = case Partition of
        {[{ContainerID, _}], Rest} ->
            Cmd = binary_to_list(iolist_to_binary(["docker kill ", ContainerID])),
            ?LOG_DEBUG("CMD: ~p",[Cmd]),
            os:cmd(Cmd),
            Rest;
        {[], Rest} ->
            Rest
    end,
    {reply, ok, S#state{containers = maps:from_list(Remaining)}};

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast({launch, Name, Opts}, #state{containers = Containers} = S) ->
    store_connections(Name, Opts),
    ContainerID = exec_docker_run(Name, Opts),
    Container = #container{
        name = Name,
        image = maps:get(<<"image">>, Opts),
        status = unknown
    },
    ?LOG_NOTICE("Started container ~p",[Name]),
    {noreply, S#state{containers = Containers#{ContainerID => Container}}};

handle_cast({log, ContainerID, Text}, #state{containers = _CTNs} = S) ->
    % TODO: store logs to query them later
    ?LOG_DEBUG("[~p]: ~s", [ContainerID, Text]),
    {noreply, S};

handle_cast({disconnect, ContainerID}, #state{containers = CTNs} = S) ->
    case CTNs of
        #{ContainerID := #container{name = N}} ->
            ?LOG_NOTICE("Container ~p lost connection.", [N]),
            {noreply, update_container_status(ContainerID, lost, S)};
        _ ->
            {noreply, S}
    end;

handle_cast({event, ContainerID, Event}, #state{containers = CTNs} = S) ->
    NewS = case maps:is_key(ContainerID, CTNs) of
        true -> handle_event(ContainerID, Event, S);
        false ->
            ?LOG_ERROR("Event from unexpected container ~p", [ContainerID]),
            S
    end,
    {noreply, NewS};

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast ~p", [Msg]),
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

store_connections(Node, #{<<"connections">> := Connections}) ->
    braidnet_epmd_server:store_connections(Node, Connections).

exec_docker_run(Name, #{<<"image">> := DockerImage, <<"epmd_port">> := Port}) ->
    ContainerId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    RunCommandParts = [
        "docker run -d",
        "--env CID=" ++ binary_to_list(ContainerId),
        "--env NODE_NAME=" ++ binary_to_list(Name),
        "--env BRD_EPMD_PORT=" ++ binary_to_list(Port),
        "--hostname " ++ net_adm:localhost() ++ ".braidnet",
        "--network host",
        binary_to_list(DockerImage)
    ],
    RunCommand = string:join(RunCommandParts, " "),
    Result = string:trim(os:cmd(RunCommand)),
    ?LOG_DEBUG("Executed ~p~nResult: ~p", [RunCommand, Result]),
    ContainerId.
