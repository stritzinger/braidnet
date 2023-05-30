-module(braidnet).

-export([test_node/0]).
-export([launch_configuration/1]).
-export([list/0]).
-export([remove_configuration/1]).
-export([pause/1]).
-export([unpause/1]).

% dev api ----------------------------------------------------------------------

test_node() ->
    launch_configuration(#{
        atom_to_binary(node()) => #{
            <<"dummy_container">> => #{
                <<"image">> => <<"local/braidnode">>,
                <<"connections" >> => []
            }
        }
    }).

launch_configuration(Config) ->
    ThisNode = atom_to_binary(node()),
    case Config of
        #{ThisNode := HostedNodes} ->
            [braidnet_container:launch(Container, Img) ||
                {Container, #{<<"image">> := Img}} <- maps:to_list(HostedNodes)];
        _ -> skip
    end,
    ok.

list() ->
    braidnet_container:list().

remove_configuration(Config) ->
    ThisNode = atom_to_binary(node()),
    case Config of
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
