-module(braidnet_container).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% External API
-export([start_link/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {cid :: string(), port :: pid()}).

start_link(Name, CID, Opts) ->
    gen_server:start_link(?MODULE, [Name, CID, Opts], []).

% gen_server callbacks ---------------------------------------------------------

init([Name, CID, #{<<"image">> := DockerImage} = Opts]) ->
    Docker = os:find_executable("docker"),
    case check_image_signature(Docker, DockerImage) of
        ok ->
            {ok, #state{cid = CID, port = docker_run(Docker, Name, CID, Opts)}};
        {error, E} ->
            ?LOG_WARNING("Rejecting unsigned image: ~p for: ~p",[DockerImage, E]),
            braidnet_orchestrator:log(CID, "brainet: docker image has no trust data"),
            {stop, E}
    end.

handle_call(Msg, _, S) ->
    ?LOG_ERROR("Unexpected call ~p", [Msg]),
    {reply, ok, S}.

handle_cast(Msg, S) ->
    ?LOG_ERROR("Unexpected cast ~p", [Msg]),
    {noreply, S}.

handle_info({Port, {data, Data}}, #state{cid = CID, port = Port} = S) ->
    braidnet_orchestrator:log(CID, Data),
    % This is just to see container logs in early development
    [?LOG_DEBUG("Container ~p: ~s", [CID, L]) || L <- string:split(Data, "\n", all)],
    {noreply, S};
handle_info({Port, {exit_status, Code}}, #state{cid = CID, port = Port} = S) ->
    Reason = evaluate_exit_code(Code),
    ?LOG_DEBUG("Container ~p, terminated with code ~p for reason: ~p",
              [CID, Code, Reason]),
    braidnet_orchestrator:disconnect(CID),
    {stop, Reason, S};
handle_info(Msg, S) ->
    ?LOG_ERROR("Unexpected info: ~p",[Msg]),
    {noreply, S}.

% INTERNAL ---------------------------------------------------------------------

check_image_signature(Docker, DockerImage) ->
    PortSettings = [
        {args, [
            "trust",
            "inspect",
            DockerImage
        ]}
    ],
    Port = erlang:open_port({spawn_executable, Docker}, PortSettings),
    Result = receive {Port, {data, Data}} ->
        jiffy:decode(Data,[return_maps])
    end,
    case Result of
        [] -> {error, no_signatures};
        _ ->
            % ?LOG_DEBUG("Trust inspect result: ~p",[Result]),
            ok
    end.

docker_run(Docker, Name, CID,
           #{<<"image">> := DockerImage, <<"epmd_port">> := Port}) ->
    NodeHost = braidnet_cluster:this_nodehost(),
    StringCID = binary_to_list(CID),
    CA = braidnet_cert:get_ca_file(),
    Cert = braidnet_cert:new_braidnode_cert(StringCID),
    PortSettings = [
        {args, [
            "run",
            "--rm",
            "-v", CA ++ ":/mnt/certs/braidcert.CA.pem",
            "-v", Cert ++ ":/mnt/certs/braidnode.pem",
            "--env", "CID=" ++ StringCID,
            "--env", "NODE_NAME=" ++ binary_to_list(Name),
            "--env", "NODE_HOST=" ++ binary_to_list(NodeHost),
            "--env", "BRD_EPMD_PORT=" ++ binary_to_list(Port),
            "--hostname", binary_to_list(NodeHost),
            "--network", "host",
            binary_to_list(DockerImage)
        ]},
        exit_status,
        stderr_to_stdout
    ],
    erlang:open_port({spawn_executable, Docker}, PortSettings).

evaluate_exit_code(0) -> normal;
evaluate_exit_code(125) -> docker_run_failure;
evaluate_exit_code(126) -> contained_command_cannot_be_invoked;
evaluate_exit_code(127) -> contained_command_cannot_be_found;
evaluate_exit_code(130) -> ctrl_c;
evaluate_exit_code(137) -> 'SIGKILL';
evaluate_exit_code(143) -> 'SIGTERM';
evaluate_exit_code(_) -> unknown.
