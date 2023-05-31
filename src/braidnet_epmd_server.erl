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
    names/1,
    register_node/1,
    port_please/1,
    address_please/1,
    connections/1
]).

-include_lib("kernel/include/logger.hrl").

% Map of maps associating Hostnames with node => port maps.
-record(state, {
    hostname :: bitstring(),
    nodes = #{} :: #{
        Hostname :: bitstring() := #{
            NodeName :: bitstring() => Port :: integer()
        }
    }
}).


register_node(#{<<"name">> := Name, <<"port">> := Port}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Port}).

address_please(#{<<"host">> := Host, <<"name">> := Name}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host, Name}).

port_please(#{<<"name">> := Name, <<"host">> := Host}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Host}).

names(#{<<"host">> := Host}) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Host}).

connections([Node]) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Node}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    [_, ThisHost] = binary:split(erlang:atom_to_binary(node()), <<"@">>),
    {ok, #state{hostname = ThisHost}}.

handle_call({register_node, NodeName, NodePort}, _, #state{nodes = Nodes0} = State) ->
    ?LOG_DEBUG("register_node: ~p~n", [{NodeName, NodePort}]),
    ThisHost = State#state.hostname,
    NodesHere0 = maps:get(ThisHost, Nodes0, #{}),
    NodesHere1 = maps:put(NodeName, NodePort, NodesHere0),
    Nodes1 = maps:put(ThisHost, NodesHere1, Nodes0),
    {reply, ok, State#state{nodes = Nodes1}};

handle_call({address_please, Host, Node}, _, State) ->
    % TODO: remote nodes
    ?LOG_INFO("address please: ~p~n", [{Host, Node}]),
    case node_ip_and_port(Host, Node, State) of
        nxdomain -> {reply, [error, nxdomain], State};
        unknown -> {reply, [error, unknown], State};
        [IP, Port] -> {reply, [ok, IP, Port], State}
    end;

handle_call({port_please, NodeName, IP}, _, #state{nodes = Nodes0} = State) ->
    ?LOG_DEBUG("asking for port: ~p~n", [{NodeName, IP}]),
    NodeHost = erlang:list_to_tuple(IP),
    NodesThere = maps:get(NodeHost, Nodes0, #{}),
    case maps:get(NodeName, NodesThere, undefined) of
        undefined ->
            ?LOG_DEBUG("NOPORT!"),
            {reply, [ok, noport], State};  % TODO
        Port -> {reply, [ok, Port], State}
    end;

handle_call({names, Host}, _, #state{nodes = Nodes0} = State) ->
    NodesThere = maps:get(Host, Nodes0, #{}),
    {reply, [ok, maps:keys(NodesThere)], State};

handle_call({connections, Node}, _, State) ->
    ?LOG_DEBUG("asking for connections: ~p~n", [Node]),
    [NodeName, _] = binary:split(Node, <<"@">>),
    {ok, NodeMap} = application:get_env(braidnet, nodemap),
    Connections = maps:get(<<"connections">>, maps:get(NodeName, NodeMap)),
    {reply, [ok, Connections], State};

handle_call(Msg, _From, S) ->
    ?LOG_ERROR("Unexpected call msg: ~p",[Msg]),
    {reply, ok, S}.


handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast msg: ~p",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected info msg: ~p",[Msg]),
    {noreply, S}.

node_ip_and_port(Host, Node, State) when is_list(Host) ->
    node_ip_and_port(erlang:list_to_binary(Host), Node, State);
node_ip_and_port(Host, Node, State) when is_list(Node) ->
    node_ip_and_port(Host, erlang:list_to_binary(Node), State);
node_ip_and_port(Host, Node, #state{nodes = NodeMap} = State) ->
    ?LOG_DEBUG("HOST: ~p, NODE: ~p", [Host, Node]),
    ThisHost = State#state.hostname,
    NodesOnHost = maps:get(Host, NodeMap, undefined),
    PortOrError = case {Host, NodesOnHost} of
        {_, undefined} ->
            nxdomain;
        {ThisHost, Nodes} when is_map(Nodes) ->
            maps:get(Node, NodesOnHost, unknown);
        _ ->
            ?LOG_DEBUG("asking for a remote address!"), % TODO
            nxdomain
    end,
    case PortOrError of
        Port when is_integer(Port) -> [[127,0,0,1], Port];
        Error -> Error
    end.
