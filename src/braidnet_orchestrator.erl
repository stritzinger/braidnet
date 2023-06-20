-module(braidnet_orchestrator).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% External API
-export([start_link/0,
         launch/2,
         list/0,
         logs/1,
         delete/1,
         get_ws_pid/1]).

% Internal API
-export([verify/1,
         connect/2,
         disconnect/1,
         log/2,
         event/2,
         sign/4]).

-include_lib("kernel/include/logger.hrl").

% custom ID to store container reference internally
-type cid() :: binary().

-record(container, {
    node_name           :: binary(),
    ws_pid              :: undefined | pid(),
    image               :: binary(),
    status              :: starting | broken | running | lost,
    logs = ""           :: string()
}).

-record(state, {
    containers  = #{} :: #{cid() => #container{}}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

launch(Name, Opts) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, Name, Opts}).

list() ->
    gen_server:call(?MODULE, ?FUNCTION_NAME).

logs(CID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, CID}).

delete(ContainerName) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerName}).

get_ws_pid(CID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, CID}).

verify(ContainerID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerID}).

connect(ContainerID, WSPid) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, WSPid}).

disconnect(ContainerID) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID}).

log(ContainerID, Text) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, Text}).

event(ContainerID, Event) ->
    gen_server:cast(?MODULE, {?FUNCTION_NAME, ContainerID, Event}).

sign(ContainerID, Payload, HashAlg, SignAlg) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, ContainerID, Payload, HashAlg, SignAlg}).

% gen_server callbacks ---------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({verify, CID}, _, #state{containers = CTNs} = S) ->
    case CTNs of
        #{CID := #container{node_name = _N}} ->
            {reply, ok, S};
        _ -> {reply, {error, unexpected_id}, S}
    end;

handle_call(list, _, #state{containers = CTNs} = S) ->
    List =
    [#{id => ID,
       name => Name,
       image => Image,
       status => Status} ||
        {ID, #container{node_name = Name, image = Image, status = Status}}
            <- maps:to_list(CTNs)],
    {reply, List, S};

handle_call({logs, CID}, _, #state{containers = CTNs} = S) ->
    #container{
        logs = Logs
    } = maps:get(CID, CTNs),
    {reply, Logs, S};

handle_call({get_ws_pid, CID}, _, #state{containers = CTNs} = S) ->
    #container{
        ws_pid = Pid
    } = maps:get(CID, CTNs),
    {reply, Pid, S};

handle_call({delete, NodeName}, _, #state{containers = CTNs} = S) ->
    Partition = lists:partition(
        fun ({_ ,#container{node_name = Name}}) when Name == NodeName -> true;
            (_) -> false
        end, maps:to_list(CTNs)),
    Remaining = case Partition of
        {[{_, #container{ws_pid = WsPid}}], Rest} when is_pid(WsPid)->
            braidnet_braidnode_api:notify(WsPid, shutdown),
            Rest;
        {_, Rest} ->
            Rest
    end,
    {reply, ok, S#state{containers = maps:from_list(Remaining)}};

handle_call({sign, CID, Payload, HashAlg, SignAlg}, _,
            #state{containers = CTNs} = S) ->
    case CTNs of
        #{CID := #container{node_name = _N}} ->
            Result = sign_container_payload(CID, Payload, HashAlg, SignAlg),
            {reply, Result, S};
        _ -> {reply, {error, unknown_container}, S}
    end;

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast({launch, Name, #{<<"image">> := Image} = Opts},
             #state{containers = Containers} = S) ->
    store_connections(Name, Opts),
    CID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    Result = supervisor:start_child(braidnet_container_pool_sup,
                                          [Name, CID, Opts]),
    Status = case Result of
        {ok, _Child} ->
            ?LOG_NOTICE("Started node ~p",[Name]),
            starting;
        {error, {shutdown, {failed_to_start_child, _, E}}} ->
            ?LOG_NOTICE("Node Start Failure ~p", [E]),
            broken
    end,
    CTN = #container{
        node_name = Name,
        image = Image,
        status = Status
    },
    {noreply, S#state{containers = Containers#{CID => CTN}}};

handle_cast({connect, CID, WSPid}, #state{containers = CTNs} = S) ->
    #{CID := #container{node_name = N} = CTN} = CTNs,
    ?LOG_NOTICE("Node ~p enstablished WS connection.", [N]),
    Container = CTN#container{ws_pid = WSPid, status = running},
    NewMap = maps:update(CID, Container, CTNs),
    {noreply, S#state{containers = NewMap}};

handle_cast({log, CID, Text}, #state{containers = CTNs} = S)  ->
    case maps:is_key(CID, CTNs) of
        true ->
            CTN = maps:get(CID, CTNs),
            NewCTN = CTN#container{logs = CTN#container.logs ++ Text},
            {noreply, S#state{containers = maps:update(CID, NewCTN, CTNs)}};
        false ->
            ?LOG_WARNING("Unexpected logs from ~p ~p", [CID, Text]),
            {noreply, S}
    end;

handle_cast({disconnect, CID}, #state{containers = CTNs} = S) ->
    case CTNs of
        #{CID := #container{node_name = N}} ->
            ?LOG_NOTICE("Node ~p lost connection.", [N]),
            #{CID := CTN} = CTNs,
            Container = CTN#container{ws_pid = undefined, status = lost},
            NewMap = maps:update(CID, Container, CTNs),
            {noreply,  S#state{containers = NewMap}};
        _ ->
            {noreply, S}
    end;

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast ~p", [Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected msg: ~p",[Msg]),
    {noreply, S}.

store_connections(Node, #{<<"connections">> := Connections}) ->
    braidnet_epmd_server:store_connections(Node, Connections).

sign_container_payload(CID, Payload, <<"sha512">>, <<"rsa_pss_rsae">>) ->
    try
        Binary = erlang:binary_to_term(base64:decode(Payload)),
        Key = get_key(CID),
        Opts = [{rsa_padding, rsa_pkcs1_pss_padding},
                {rsa_pss_saltlen, -1},
                {rsa_mgf1_md, sha512}],
        Signature = public_key:sign(Binary, sha512, Key, Opts),
        base64:encode(erlang:term_to_binary(Signature))
    catch error:E ->
        ?LOG_ERROR("Error signing key: ~p", [E]),
        #{error => E}
    end.

get_key(CID) ->
    KeyFile = braidnet_cert:get_private_key_file(CID),
    {ok, PemBin} = file:read_file(KeyFile),
    [PrivateKey] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(PrivateKey).
