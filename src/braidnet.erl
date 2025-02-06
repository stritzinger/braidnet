-module(braidnet).

-export([
    test_node/0,
    test_nodes/0,
    test_node_fly/1
]).

-export([instances/0]).
-export([launch_configuration/1]).
-export([list/0]).
-export([logs/1]).
-export([rpc/4]).
-export([remove_configuration/1]).

-include_lib("kernel/include/logger.hrl").

% dev api ----------------------------------------------------------------------

test_node() ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"dummy_container">> => #{
            <<"image">> => <<"ntshtng/braidnode:signed">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

test_nodes() ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"n1">> => #{
            <<"image">> => <<"ntshtng/braidnode:signed">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections">> => [<<"n2@", ThisHost/binary>>]
        },
        <<"n2">> => #{
            <<"image">> => <<"ntshtng/braidnode:signed">>,
            <<"epmd_port">> => <<"43592">>,
            <<"connections">> => [<<"n1@", ThisHost/binary>>]
        },
        <<"dummy_container">> => #{
            <<"image">> => <<"ntshtng/braidnode:signed">>,
            <<"epmd_port">> => <<"43593">>,
            <<"connections" >> => []
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

test_node_fly(RemoteMachine) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    NodeMap = #{
        <<"braidnode">> => #{
            <<"image">> => <<"ntshtng/braidnode:signed">>,
            <<"epmd_port">> => <<"43591">>,
            <<"connections" >> => [<<"braidnode@", RemoteMachine/binary>>]
        }
    },
    launch_configuration(#{ThisHost => NodeMap}).

instances() ->
    [list_to_binary(M) || M <- braidnet_cluster:fly_machines()].

launch_configuration(NodesMap) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    LaunchHere = maps:get(ThisHost, NodesMap, #{}),
    maps:foreach(fun braidnet_orchestrator:launch/2, LaunchHere).

list() ->
    braidnet_orchestrator:list().

logs(CID) ->
    braidnet_orchestrator:logs(CID).

rpc(CID, M, F, A)
  when is_atom(M), is_atom(F), is_list(A) orelse A =:= undefined ->
    M2 = base64:encode(term_to_binary(M)),
    F2 = base64:encode(term_to_binary(F)),
    A2 = case A =:= undefined of
        true -> undefined;
        false -> base64:encode(term_to_binary(A))
    end,
    case rpc(CID, M2, F2, A2) of
        {ok, Result} -> binary_to_term(base64:decode(Result));
        {error, _Reason} = Error -> Error
    end;
rpc(CID, M, F, A)
  when is_binary(M), is_binary(F), is_binary(A) orelse A =:= undefined ->
    case braidnet_orchestrator:get_ws_pid(CID) of
        undefined ->
            {error, no_connection};
        Pid ->
            Params = #{m => M, f => F, a => A},
            braidnet_braidnode_api:request(Pid, rpc, Params)
    end.

remove_configuration(NodesMap) ->
    ThisHost = braidnet_cluster:this_nodehost(),
    ToBeDestroyed = maps:get(ThisHost, NodesMap, #{}),
    Names = [Name || {Name, _} <- maps:to_list(ToBeDestroyed)],
    lists:foreach(fun braidnet_orchestrator:delete/1, Names).

% Internal ---------------------------------------------------------------------
