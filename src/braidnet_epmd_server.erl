-module(braidnet_epmd_server).

-behaviour(gen_server).

% gen_server API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% Braidnode EPMD API
-export([
    register_node/1,
    address_please/1,
    names/1
]).

-export([store_connections/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    hostname :: bitstring(),
    % Map of maps associating Hostnames with node => port maps:
    nodes = #{} :: #{
        Hostname :: bitstring() := #{
            NodeName :: bitstring() => Port :: integer()
        }
    },
    % Map storing which nodes a given node should connect to:
    connections = #{} :: #{Node :: bitstring() := [Node :: bitstring()]}
}).

store_connections(Node, Connections) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Node, Connections}).

register_node(#{<<"name">> := Name, <<"port">> := Port}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Port}).

address_please(#{<<"host">> := Host, <<"name">> := Name}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host, Name}).

names(#{<<"host">> := Host, <<"node">> := Node}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host, Node}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    ThisHost = erlang:list_to_binary(net_adm:localhost()),
    Nodes = #{ThisHost => #{}},
    {ok, #state{hostname = ThisHost, nodes = Nodes}}.

handle_call({store_connections, Node, NodeConns}, _, State) ->
    #state{connections = Connections0} = State,
    Connections1 = maps:put(Node, NodeConns, Connections0),
    {reply, ok, State#state{connections = Connections1}};

handle_call({register_node, Name, Port}, _, #state{nodes = NodeMap0} = State0) ->
    NodesHere0 = maps:get(State0#state.hostname, NodeMap0),
    NodesHere1 = NodesHere0#{Name => Port},
    NodeMap1 = NodeMap0#{State0#state.hostname => NodesHere1},
    State1 = State0#state{nodes = NodeMap1},
    % ---
    Connections = node_connections(Name, State1#state.connections),
    % ---
    ?LOG_NOTICE("Node ~p registered with Braidnet EPMD.", [Name]),
    {reply, [ok, Connections], State1};

handle_call({address_please, Host, Node}, _, State) ->
    case node_ip_and_port(Host, Node, State) of
        nxdomain -> {reply, [error, nxdomain], State};
        unknown -> {reply, [error, unknown], State};
        [IP, Port] -> {reply, [ok, IP, Port], State}
    end;

handle_call({names, Host, Node}, _, #state{nodes = Nodes} = State) ->
    BitstringHost = list_to_binary(Host),
    NodesThere = maps:get(BitstringHost, Nodes, #{}),
    [NodeShort | _] = binary:split(Node, <<"@">>),
    CanSee = filter_visible_nodes(NodeShort, NodesThere,
                                  BitstringHost, State#state.connections),
    % TODO: check if node is alive.
    {reply, [ok, CanSee], State};

handle_call(Msg, _From, S) ->
    ?LOG_ERROR("Unexpected call msg: ~p",[Msg]),
    {reply, ok, S}.

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast msg: ~p",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected info msg: ~p",[Msg]),
    {noreply, S}.

% ------------------------------------------------------------------------------
node_ip_and_port(Host, Node, State) when is_list(Host) ->
    node_ip_and_port(erlang:list_to_binary(Host), Node, State);
node_ip_and_port(Host, Node, State) when is_list(Node) ->
    node_ip_and_port(Host, erlang:list_to_binary(Node), State);
node_ip_and_port(Host, Node, #state{nodes = NodeMap} = State) ->
    ThisHost = State#state.hostname,
    NodesOnHost = maps:get(Host, NodeMap, undefined),
    PortOrError = case {Host, NodesOnHost} of
        {_, undefined} ->
            nxdomain;
        {ThisHost, Nodes} when is_map(Nodes) ->
            maps:get(Node, NodesOnHost, unknown);
        _ ->
            ?LOG_DEBUG("asking for a remote address!"),
            nxdomain
    end,
    case PortOrError of
        Port when is_integer(Port) -> [[127,0,0,1], Port];
        Error -> Error
    end.

node_connections(Node, Connections) ->
    maps:get(Node, Connections).

filter_visible_nodes(Node, NodesThere, Host, Connections) ->
    NodeConnections = node_connections(Node, Connections),
    maps:filter(fun(N, _) ->
        FullName = <<N/binary, "@", Host/binary>>,
        lists:member(FullName, NodeConnections)
    end, NodesThere).
