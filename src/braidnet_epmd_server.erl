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
    register_node/2,
    address_please/2,
    names/2
]).

-export([store_connections/2]).

-include_lib("kernel/include/logger.hrl").

%-------------------------------------------------------------------------------
% The name part of a node(), without the @host part:
-type nodename()     :: binary().
% The host part of a node():
-type nodehost()     :: binary().
% A node() in binary format:
-type nodenamehost() :: binary().

%-------------------------------------------------------------------------------
-record(state, {
    % Map storing the distribution listen ports of local nodes:
    nodes = #{}       :: #{nodename() := port()},
    % Map storing which nodes a given local node should be able to connect to:
    connections = #{} :: #{nodename() := [nodenamehost()]}
}).

%-------------------------------------------------------------------------------
% Store which other Braidnodes a given Braidnode should be able to connect to.
% We'll send this list to the node when it registers with this module.
-spec store_connections(Node, Connections) -> Result when
    Node :: nodename(),
    Connections :: [nodenamehost()],
    Result :: ok.
store_connections(Node, Connections) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Node, Connections}).

% Store a locally running Braidnode's name and its listen port.
% We'll forward the port to other Braidnodes when they ask about how to
% connect to this one using address_please/2.
-spec register_node(Name, Port) -> Result when
    Name :: nodename(),
    Port :: port(),
    Result :: {ok, Connections :: [nodehost()]}.
register_node(Name, Port) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Name, Port}).

% A Braidnode wants to connect to another and needs its IP.
% Send this request to the gen_server running on the host in question.
% We'll reply with the IP and the port of the target Braidnode.
-spec address_please(Name, Host) -> Result | Error when
    Name   :: nodename(),
    Host   :: nodehost(),
    Result :: {ok, IP :: string(), port()},
    Error  :: {error, Reason :: atom()}.
address_please(Name, Host) ->
    Braidnet = braidnet_node_on_host(Host),
    try gen_server:call({?MODULE, Braidnet}, {?FUNCTION_NAME, Name, Host}, 5000)
    catch E:R:_S ->
        logger:error("Unreachable node ~p: ~n~p~n", [Braidnet, {E, R}]),
        [error, timeout]
    end.

% Returns the node names (and their ports) registered by Braidnet on a host.
% Note that this also includes nodes which this Braidnode did not connect to.
-spec names(Node, Host) -> Result | Error when
    Node :: nodenamehost(),
    Host :: binary(), % Note that this is the OS hostname, not a nodehost()
    Result :: {ok, #{nodename() := port()}},
    Error  :: {error, Reason :: atom()}.
names(Node, Host) ->
    Braidnet = braidnet_node_on_host(Host),
    try gen_server:call({?MODULE, Braidnet}, {?FUNCTION_NAME, Host, Node}, 5000)
    catch E:R:_S ->
        logger:error("Unreachable node ~p: ~n~p~n", [Braidnet, {E, R}]),
        [error, timeout]
    end.

%-------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #state{}}.

handle_call({store_connections, Node, NodeConns}, _, State) ->
    NewConns = maps:put(Node, NodeConns, State#state.connections),
    {reply, ok, State#state{connections = NewConns}};

handle_call({register_node, Name, Port}, _, State) ->
    NewNodes = maps:put(Name, Port, State#state.nodes),
    % ---
    Connections = maps:get(Name, State#state.connections),
    % ---
    ?LOG_NOTICE("Node ~p registered with Braidnet EPMD.", [Name]),
    {reply, {ok, Connections}, State#state{nodes = NewNodes}};

handle_call({address_please, Name, Host}, _, State) ->
    case node_ip_and_port(Name, Host, State#state.nodes) of
        nxdomain ->
            {reply, {error, nxdomain}, State};
        noport ->
            {reply, {error, noport}, State};
        {IP, Port} ->
            Reply = {ok, inet:ntoa(IP), Port},
            {reply, Reply, State}
    end;

handle_call({names, _Host, _Node}, _, State) ->
    % TODO: check if node is alive.
    {reply, {ok, State#state.nodes}, State};

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
-spec node_ip_and_port(Name, Host, NodeMap) -> Result | Error when
    Name    :: nodename(),
    Host    :: nodehost(),
    NodeMap :: #{nodename() := port()},
    Result  :: {inet:ip_address(), port()},
    Error   :: atom().
node_ip_and_port(Name, <<"braid.local">>, NodeMap) ->
    % Temporary solution for local development
    case maps:get(Name, NodeMap, undefined) of
        undefined -> noport;
        Port -> {{0,0,0,0,0,0,0,1}, Port}
    end;
node_ip_and_port(Name, Host, NodeMap) ->
    HostStr = erlang:binary_to_list(Host),
    case {inet:getaddr(HostStr, inet6), maps:get(Name, NodeMap, undefined)} of
        {{error, _}, _} ->
            nxdomain;
        {_, undefined} ->
            noport;
        {{ok, IP}, Port} ->
            {IP, Port}
    end.

-spec braidnet_node_on_host(nodehost()) -> node().
braidnet_node_on_host(Host) ->
    NodeName = braidnet_cluster:this_nodename(),
    erlang:binary_to_atom(<<NodeName/binary, "@", Host/binary>>).
