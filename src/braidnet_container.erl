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

init([Name, CID, #{<<"image">> := DockerImage, <<"epmd_port">> := Port}]) ->
    Docker = os:find_executable("docker"),
    NodeHost = braidnet_cluster:this_nodehost(),
    % Just for testing; each container should get its own certs in a dedicated directory
    TestCertsDir = filename:join([code:priv_dir(braidnet), "_dev_certs"]),
    PortSettings = [
        {args, [
            "run",
            "--rm",
            "-v", TestCertsDir ++ ":/mnt/certs",
            "--env", "CID=" ++ binary_to_list(CID),
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
    ErlangPort = erlang:open_port({spawn_executable, Docker}, PortSettings),
    {ok, #state{cid = CID, port = ErlangPort}}.

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


evaluate_exit_code(0) -> normal;
evaluate_exit_code(125) -> docker_run_failure;
evaluate_exit_code(126) -> contained_command_cannot_be_invoked;
evaluate_exit_code(127) -> contained_command_cannot_be_found;
evaluate_exit_code(130) -> ctrl_c;
evaluate_exit_code(137) -> 'SIGKILL';
evaluate_exit_code(143) -> 'SIGTERM';
evaluate_exit_code(_) -> unknown.
