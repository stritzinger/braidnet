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

handle_cast({launch, Name, #{<<"image">> := DockerImage, <<"epmd_port">> := Port}},
            #state{containers = Containers} = S) ->
    [_, ThisHost] = binary:split(erlang:atom_to_binary(node()), <<"@">>),
    CID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Cmd = string:join([
        "docker run -d",
        "--env CID=" ++ binary_to_list(CID),
        "--env NODE_NAME=" ++ binary_to_list(<<Name/binary, "@", ThisHost/binary>>),
        "--env BRD_EPMD_PORT=" ++ binary_to_list(Port),
        "--network host",
        binary_to_list(DockerImage)
    ], " "),
    ?LOG_DEBUG("CMD: ~p",[Cmd]),

    CmdResult = string:trim(os:cmd(Cmd)),
    ?LOG_DEBUG("CMD result: ~p",[CmdResult]),
    ?LOG_NOTICE("Started container ~p",[Name]),

    Container = #container{name = Name, image = DockerImage, status = unknown},
    {noreply, S#state{containers = Containers#{CID => Container}}};

handle_cast({event, ContainerID, Event}, #state{containers = CTNs} = S) ->
    NewS = case maps:is_key(ContainerID, CTNs) of
        true -> handle_event(ContainerID, Event, S);
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
