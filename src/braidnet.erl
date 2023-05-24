-module(braidnet).

-export([launch_node/0]).

% dev api ----------------------------------------------------------------------

launch_node() ->
    braidnet_container:launch("local/braidnode").
