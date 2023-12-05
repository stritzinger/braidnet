-module(braidnet_monitor).

-export([init/0]).

-record(braidnet_monitor, {
    instance :: binary(),
    properties = #{} :: #{
        config => map()
    }
}).

init() ->
    mnesia:create_table(braidnet_monitor, [
        {attributes, record_info(fields, braidnet_monitor)},
        {ram_cpoies, [node() | nodes()]}
    ]),
    Instance = braidnet_cluster:this_nodehost(),
    mnesia:transaction(fun() ->
        mnesia:write(#braidnet_monitor{instance = Instance})
    end).
