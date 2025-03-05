-module(braidnet_container).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

% External API
-export([start_link/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {cid :: binary(), port :: port()}).

start_link(Name, CID, Opts) ->
    gen_server:start_link(?MODULE, [Name, CID, Opts], []).

% gen_server callbacks ---------------------------------------------------------

init([Name, CID, #{<<"image">> := DockerImage} = Opts]) ->
    Envs = maps:get(<<"envs">>, Opts, []),
    SignCheck = check_image_signature(DockerImage),
    PortReservation = braidnet_epmd_server:reserve_port(self()),
    case {SignCheck, PortReservation} of
        {ok, {ok, N}} ->
            DockerPort = docker_run(Name, CID, DockerImage, N, Envs),
            State = #state{cid = CID, port = DockerPort},
            {ok, State};
        {{error, E}, _} ->
            ?LOG_DEBUG("Rejecting unsigned image: ~p for: ~p",[DockerImage, E]),
            braidnet_orchestrator:log(CID, "brainet: docker image has no trust data"),
            {stop, E};
        {_, {error, E}} ->
            ?LOG_DEBUG("Error reserving port: ~p",[E]),
            braidnet_orchestrator:log(CID, "brainet: could not reserve port for container"),
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

check_image_signature(DockerImage) ->
    case application:get_env(braidnet, docker_trust) of
        {ok, false} ->
            ?LOG_WARNING("Docker content trust has been disabled!"),
            ok;
        {ok, true} ->
            do_check_image_signature(DockerImage)
    end.

do_check_image_signature(DockerImage) ->
    Docker = os:find_executable("docker"),
    PortSettings = [
        {env,[docker_content_trust_env()]},
        {args, [
            % pulling will fail if the image sign data is empty or unstrusted
            "pull",
            DockerImage
        ]},
        exit_status,
        stderr_to_stdout
    ],
    Port = erlang:open_port({spawn_executable, Docker}, PortSettings),
    Result = receive
        {Port, {exit_status, Code}} -> evaluate_exit_code(Code)
    end,
    case Result of
        normal ->
            ?LOG_DEBUG("Trust result: ~p",[Result]),
            ok;
        _ -> {error, invalid_signatures}
    end.

docker_content_trust_env() ->
    case application:get_env(braidnet, docker_trust) of
        {ok, false} -> {"DOCKER_CONTENT_TRUST", "0"};
        {ok, true} -> {"DOCKER_CONTENT_TRUST", "1"}
    end.

docker_run(Name, CID, DockerImage, PortNumber, Envs) ->
    Docker = os:find_executable("docker"),
    NodeHost = braidnet_cluster:this_nodehost(),
    StringCID = binary_to_list(CID),
    BraidCAFile = braidnet_cert:get_ca_file(),
    StritzingerGrispCAFile = braidnet_cert:get_stritzinger_ca_file(),
    BriadCertFile = braidnet_cert:new_braidnode_cert(StringCID),

    {ok, BraidCA} = file:read_file(BraidCAFile),
    {ok, StritzingerGrispCA} = file:read_file(StritzingerGrispCAFile),
    MergedCAs = <<BraidCA/binary, "\n", StritzingerGrispCA/binary>>,
    MergedCAsFile = filename:join([braidnet_cert:cert_dir_path(CID), "CA_certs.pem"]),
    ok = file:write_file(MergedCAsFile, MergedCAs),

    Args = [
        "run",
        "--rm",
        "-v", binary_to_list(MergedCAsFile) ++ ":/mnt/certs/CA_certs.pem",
        "-v", BriadCertFile ++ ":/mnt/certs/braidnode.pem",
        "--env", "CID=" ++ StringCID,
        "--env", "NODE_NAME=" ++ binary_to_list(Name),
        "--env", "NODE_HOST=" ++ binary_to_list(NodeHost),
        "--env", "BRD_EPMD_PORT=" ++ integer_to_list(PortNumber),
        "--hostname", binary_to_list(NodeHost),
        "--network", "host",
        "--pull", "always" % Just for development, should be user configured
    ] ++ parse_user_envs(Envs) ++ [binary_to_list(DockerImage)],

    PortSettings = [
        {env, [docker_content_trust_env()]},
        {args, Args},
        exit_status,
        stderr_to_stdout
    ],
    erlang:open_port({spawn_executable, Docker}, PortSettings).

parse_user_envs(Envs) ->
    lists:foldl(fun({K, V}, Acc) ->
        case prohibited_user_env_key(K) of
            true ->
                error("Prohibited user env key: ~p", [K]);
            false ->
                ["--env", binary_to_list(K) ++ "=" ++ binary_to_list(V) | Acc]
        end
    end, [], Envs).

prohibited_user_env_key(Key) ->
    ProhibitedKeys = ["CID", "NODE_NAME", "NODE_HOST", "BRD_EPMD_PORT"],
    lists:member(binary_to_list(Key), ProhibitedKeys).

evaluate_exit_code(0) -> normal;
evaluate_exit_code(125) -> docker_run_failure;
evaluate_exit_code(126) -> contained_command_cannot_be_invoked;
evaluate_exit_code(127) -> contained_command_cannot_be_found;
evaluate_exit_code(130) -> ctrl_c;
evaluate_exit_code(137) -> 'SIGKILL';
evaluate_exit_code(143) -> 'SIGTERM';
evaluate_exit_code(_) -> unknown.
