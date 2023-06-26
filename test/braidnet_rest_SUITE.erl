-module(braidnet_rest_SUITE).

-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

% Test cases:
-export([
    list/1
]).

-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-define(braidnode_image, <<"ziopio/braidnode:testing">>).


%--- CT Callbacks --------------------------------------------------------------

all() ->
    [list].

suite() ->
    [
        {timetrap, {minutes, 2}},
        % Only run if the entry 'braid' is present in the test.config file:
        {require, braid}
    ].

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

init_per_testcase(_, Config) ->
    LaunchConfig = example_config(Config),
    braid_rest:launch(LaunchConfig),
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
            Response = braid_rest:destroy(LaunchConfig),
            ?assertMatch([{_, {204, _}}], Response)
    end.

%--- Tests ---------------------------------------------------------------------

%% Test that listing the running configuration works.
list(Config) ->
    LaunchConfig = proplists:get_value(launch_config, Config),
    Response = braid_rest:list(LaunchConfig),
    [Machine| _] = maps:keys(LaunchConfig),
    ?assertMatch([{Machine, {200, [
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

example_config(CtConfig) ->
    [Machine1 | _] = ?config(machines, CtConfig),
    % A configuration with two nodes on one machine, connected.
    N1Node = erlang:binary_to_atom(<<"n1@", Machine1/binary>>),
    N2Node = erlang:binary_to_atom(<<"n2@", Machine1/binary>>),
    #{
        Machine1 =>
            #{
                n1 => #{
                    image => ?braidnode_image,
                    connections => [N2Node]
                },
                n2 => #{
                    image => ?braidnode_image,
                    connections => [N1Node]
                }
            }
    }.
>>>>>>> 146455d (simplify test suite)
