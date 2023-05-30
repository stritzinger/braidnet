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
    nodes = #{} :: #{
        Hostname :: bitstring() := #{
            NodeName :: bitstring() => Port :: integer()
        }
    }
}).


register_node(Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Params}).

address_please(Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Params}).

port_please(Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Params}).

names(Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Params}).

connections(Params) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, Params}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({register_node, Params}, _, #state{nodes = Nodes0} = State) ->
    ?LOG_DEBUG("register_node: ~p~n", [Params]),
    HostName = {127,0,0,1},  % TODO: don't hardcode this
    NodeName = maps:get(<<"name">>, Params),
    NodePort = maps:get(<<"port">>, Params),
    % ---
    NodesHere0 = maps:get(HostName, Nodes0, #{}),
    NodesHere1 = maps:put(NodeName, NodePort, NodesHere0),
    Nodes1 = maps:put(HostName, NodesHere1, Nodes0),
    % ---
    {reply, ok, State#state{nodes = Nodes1}};

handle_call({address_please, _Params}, _, State) ->
    % TODO: remote nodes
    {reply, [ok, [127,0,0,1]], State};

handle_call({port_please, Params}, _, #state{nodes = Nodes0} = State) ->
    ?LOG_DEBUG("asking for port: ~p~n", [Params]),
    NodeName = maps:get(<<"name">>, Params),
    NodeHost = erlang:list_to_tuple(maps:get(<<"host">>, Params)),
    NodesThere = maps:get(NodeHost, Nodes0, #{}),
    case maps:get(NodeName, NodesThere, undefined) of
        undefined -> {reply, [ok, noport], State};  % TODO
        Port -> {reply, [ok, Port], State}
    end;

handle_call({names, #{<<"host">> := Host}}, _, #state{nodes = Nodes0} = State) ->
    NodesThere = maps:get(Host, Nodes0, #{}),
    {reply, [ok, maps:keys(NodesThere)], State};

handle_call({connections, [Node]}, _, State) ->
    ?LOG_DEBUG("asking for connections: ~p~n", [Node]),
    {ok, NodeMap} = application:get_env(braidnet, nodemap),
    Connections = maps:get(<<"connections">>, maps:get(Node, NodeMap)),
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
