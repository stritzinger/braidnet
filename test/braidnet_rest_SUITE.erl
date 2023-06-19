-module(braidnet_rest_SUITE).

-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

% Test cases:
-export([
    braidnet_rest_001/1,
    braidnet_rest_002/1,
    braidnet_rest_003/1
]).

-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-record(launchConfig, {
    map, % The configuration itself, as a map
    path % Path to the file containing the above map
}).

-define(braidnode_image, <<"ziopio/braidnode:testing">>).

%--- Tests ---------------------------------------------------------------------
%% Test that Braidnet can accept and process a launch request.
braidnet_rest_001(Config) ->
    LaunchConfig = ?config(launch_config, Config),
    Response = braid_rest:launch(LaunchConfig#launchConfig.path),
    ?assertMatch([{_, {202, _}}], Response).

%% Test that Braidnet can accept and process a removal request.
braidnet_rest_002(Config) ->
    LaunchConfig = ?config(launch_config, Config),
    Response = braid_rest:destroy(LaunchConfig#launchConfig.path),
    ?assertMatch([{_, {202, _}}], Response).

%% Test that listing the running configuration works.
braidnet_rest_003(Config) ->
    LaunchConfig = proplists:get_value(launch_config, Config),
    Response = braid_rest:list(LaunchConfig#launchConfig.path),
    [Machine] = maps:keys(LaunchConfig#launchConfig.map),
    MachineBin = erlang:atom_to_binary(Machine),
    ?assertMatch([{MachineBin, {200, [
        #{
            <<"id">> := _,
            <<"image">> := _,
            <<"name">> := _,
            <<"status">> := _
        },
        #{
            <<"id">> := _,
            <<"image">> := _,
            <<"name">> := _,
            <<"status">> := _
        }
    ]}}], Response),
    [{_, {_, [#{<<"status">> := S1}, #{<<"status">> := S2}]}}] = Response,
    ?assertNotMatch(<<"broken">>, S1),
    ?assertNotMatch(<<"broken">>, S2).

%--- Helpers -------------------------------------------------------------------
% Writes a launch configuration to file,
% to be used through the braidnet_rest module.
% Returns the file path and the configuration too as a #launchConfig{}.
write_launch_config(Id, CtConfig) ->
    PrivDir = ?config(priv_dir, CtConfig),
    Machines = ?config(machines, CtConfig),
    ConfigMap = example_config(Id, Machines),
    ConfigPath = filename:join(PrivDir, "launch.conf"),
    ok = file:write_file(ConfigPath, io_lib:format("~p.~n", [ConfigMap])),
    #launchConfig{map = ConfigMap, path = ConfigPath}.

example_config(1, [Machine1 | _]) ->
    % A configuration with one node on one machine, no connections
    MachineAtom = erlang:binary_to_atom(Machine1),
    #{
        MachineAtom =>
            #{
                n1 => #{
                    image => ?braidnode_image,
                    epmd_port => <<"43591">>,
                    connections => []
                }
            }
    };
example_config(2, [Machine1 | _]) ->
    % A configuration with two nodes on one machine, connected.
    MachineAtom = erlang:binary_to_atom(Machine1),
    N1Node = erlang:binary_to_atom(<<"n1@", Machine1/binary>>),
    N2Node = erlang:binary_to_atom(<<"n2@", Machine1/binary>>),
    #{
        MachineAtom =>
            #{
                n1 => #{
                    image => ?braidnode_image,
                    epmd_port => <<"43591">>,
                    connections => [N2Node]
                },
                n2 => #{
                    image => ?braidnode_image,
                    epmd_port => <<"43592">>,
                    connections => [N1Node]
                }
            }
    }.

%--- CT Callbacks --------------------------------------------------------------
init_per_suite(Config) ->
    %--- Configure Braid
    AccessToken = os:getenv("BRAIDNET_ACCESS_TOKEN"),
    BraidConfig = ct:get_config(braid),
    application:set_env(braid, braidnet_access_token,
                        list_to_binary(AccessToken)),
    application:set_env(braid, scheme,
                        maps:get(scheme, BraidConfig)),
    application:set_env(braid, braidnet_domain,
                        maps:get(braidnet_domain, BraidConfig)),
    application:set_env(braid, port,
                        maps:get(port, BraidConfig)),
    %--- Get Fly.io machine names
    Machines = braidnet_test_utils:fly_machine_names(),
    %---
    [{machines, Machines} | Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(braidnet_rest_003, Config) ->
    LaunchConfig = write_launch_config(2, Config),
    braid_rest:launch(LaunchConfig#launchConfig.path),
    [{launch_config, LaunchConfig} | Config];

init_per_testcase(_, Config) ->
    LaunchConfig = write_launch_config(1, Config),
    braid_rest:launch(LaunchConfig#launchConfig.path),
    [{launch_config, LaunchConfig} | Config].

end_per_testcase(braidnet_rest_002, Config) ->
    case ?config(tc_status, Config) of
        {failed, _} ->
            braidnet_test_utils:fly_restart_app();
        _ ->
            ok
    end;

end_per_testcase(_, Config) ->
    case ?config(tc_status, Config) of
        {failed, _} ->
            braidnet_test_utils:fly_restart_app();
        _ ->
            LaunchConfig = ?config(launch_config, Config),
            Response = braid_rest:destroy(LaunchConfig#launchConfig.path),
            ?assertMatch([{_, {202, _}}], Response)
    end.

suite() ->
    [
        {timetrap, {minutes, 2}},
        % Only run if the entry 'braid' is present in the test.config file:
        {require, braid}
    ].

all() ->
    [braidnet_rest_001, braidnet_rest_002, braidnet_rest_003].