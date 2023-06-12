-module(braidnet).

-export([
    test_node/0,
    test_nodes/0,
    test_node_fly/1
]).

-export([launch_configuration/1]).
-export([list/0]).
-export([logs/1]).
-export([remove_configuration/1]).
-export([pause/1]).
-export([unpause/1]).

-include_lib("kernel/include/logger.hrl").

% dev api ----------------------------------------------------------------------
test_node() ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"dummy_container">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

test_nodes() ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"n1">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections">> => [<<"n2@", ThisHost/binary>>]
        },
        <<"n2">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43592">>,
            <<"connections">> => [<<"n1@", ThisHost/binary>>]
        },
        <<"dummy_container">> => #{
            <<"image">> => <<"local/braidnode">>,
            <<"epmd_port">> => <<"43593">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

test_node_fly(RemoteMachine) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"braidnode">> => #{
            <<"image">> => <<"ntshtng/braidnode">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections" >> => [<<"braidnode@", RemoteMachine/binary>>]
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

launch_configuration(NodesMap) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    LaunchHere = maps:get(ThisHost, NodesMap, #{}),
    maps:foreach(fun braidnet_orchestrator:launch/2, LaunchHere).

list() ->
    braidnet_orchestrator:list().

logs(CID) ->
    braidnet_orchestrator:logs(CID).

remove_configuration(NodesMap) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    ToBeDestroyed = maps:get(ThisHost, NodesMap, #{}),
    Names = [Name || {Name, _} <- maps:to_list(ToBeDestroyed)],
    lists:foreach(fun braidnet_orchestrator:delete/1, Names).

pause(_Containers) ->
    ok.

unpause(_Containers) ->
    ok.

% Internal ---------------------------------------------------------------------
