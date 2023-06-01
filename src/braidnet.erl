-module(braidnet).

-export([
    test_node/0,
    test_nodes/0
]).

-export([launch_configuration/1]).
-export([list/0]).
-export([remove_configuration/1]).
-export([pause/1]).
-export([unpause/1]).

-include_lib("kernel/include/logger.hrl").

% dev api ----------------------------------------------------------------------
test_node() ->
    Localhost = erlang:list_to_binary(net_adm:localhost()),
    NodeMap = #{
        <<"dummy_container">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{Localhost => NodeMap}).

test_nodes() ->
    Localhost = erlang:list_to_binary(net_adm:localhost()),
    NodeMap = #{
        <<"n1">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections">> => [<<"n2@", Localhost/binary, ".braidnet">>]
        },
        <<"n2">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43592">>,
            <<"connections">> => [<<"n1@", Localhost/binary, ".braidnet">>]
        },
        <<"dummy_container">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43593">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{Localhost => NodeMap}).

launch_configuration(NodesMap) ->
    {ok, ThisHost} = application:get_env(braidnet, hostname),
    LaunchHere = maps:get(ThisHost, NodesMap, #{}),
    maps:foreach(fun braidnet_container:launch/2, LaunchHere).

list() ->
    braidnet_container:list().

remove_configuration(NodesMap) ->
    ThisNode = atom_to_binary(node()),
    case NodesMap of
        #{ThisNode := HostedNodes} ->
            [braidnet_container:delete(Container) ||
                {Container, _} <- maps:to_list(HostedNodes)],
                ok;
        _ ->
            ?LOG_WARNING("Received a braid configuration"
                         " that does not lists this orchestrator."),
            skip
    end.

pause(Containers) ->
    ok.

unpause(Containers) ->
    ok.
