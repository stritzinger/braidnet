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

% dev api ----------------------------------------------------------------------
test_node() ->
    launch_configuration(#{
        atom_to_binary(node()) => #{
            <<"dummy_container@127.0.0.1">> => #{
                <<"image">> => <<"local/braidnode">>,
                <<"epmd_port">> => <<"43591">>,
                <<"connections" >> => []
            }
        }
    }).

test_nodes() ->
    {ok, NodeMap} = application:get_env(braidnet, nodemap),
    Nodes = #{
        atom_to_binary(node()) => NodeMap
    },
    launch_configuration(Nodes).

launch_configuration(NodesMap) ->
    ThisNode = atom_to_binary(node()),
    case NodesMap of
        #{ThisNode := HostedNodes} ->
            [braidnet_container:launch(Container, Opts) ||
                {Container, Opts} <- maps:to_list(HostedNodes)];
        _ -> skip
    end,
    ok.

list() ->
    braidnet_container:list().

remove_configuration(NodesMap) ->
    ThisNode = atom_to_binary(node()),
    case NodesMap of
        #{ThisNode := HostedNodes} ->
            [braidnet_container:delete(Container) ||
                {Container, _} <- maps:to_list(HostedNodes)];
        _ -> skip
    end,
    ok.

pause(Containers) ->
    ok.

unpause(Containers) ->
    ok.
