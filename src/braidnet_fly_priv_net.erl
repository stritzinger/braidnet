-module(braidnet_fly_priv_net).

-export([discover/0, ping_machines/0]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/logger.hrl").

-define(app_name, "braidnet-ams").

% https://fly.io/docs/reference/regions/#fly-io-regions
-define(fly_regions, [
    "ams", "arn", "atl", "bog", "bos", "cdg", "den", "dfw", "ewr", "eze", "fra",
    "gdl", "gig", "gru", "hkg", "iad", "jnb", "lax", "lhr", "maa", "mad", "mia",
    "nrt", "ord", "otp", "qro", "scl", "sea", "sin", "sjc", "syd", "waw", "yul",
    "yyz"
]).

discover() ->
    Hostnames = get_fly_hostnames(),
    HostnameToIp = get_fly_ips(Hostnames),
    add_hosts_to_inet(HostnameToIp),
    ping_machines(),
    ok.

ping_machines() ->
    Hostnames0 = get_fly_hostnames(),
    {ok, ThisNode} = inet:gethostname(),
    Hostnames1 = lists:delete(ThisNode, Hostnames0),
    PingFun = fun(Host) ->
        OtherNode = erlang:list_to_atom("braidnet" ++ "@" ++ Host),
        PingRes = net_adm:ping(OtherNode),
        ?LOG_NOTICE("braidnet@~s pings ~p: ~p~n", [ThisNode, OtherNode, PingRes])
    end,
    lists:foreach(PingFun, Hostnames1).

% ------------------------------------------------------------------------------
get_fly_hostnames() ->
    % TODO: Why is this a string inside a list inside a list?
    %       Would we get two lists if we had machines in two regions?
    #hostent{h_addr_list = [[VmsString]]} = fly_ns_vms(),
    VmsWithRegions = string:split(VmsString, ","),
    Hostnames = lists:map(
        fun(Vm) ->
            [Hostname, _Region] = string:split(Vm, " "),
            Hostname
        end,
        VmsWithRegions
    ),
    Hostnames.

-spec get_fly_ips([string()]) -> #{string() := inet:ip6_address()}.
get_fly_ips(Hostnames) ->
    lists:foldl(
        fun(Hostname, Map) ->
            #hostent{h_addr_list = [Address]} = fly_ns_vm(Hostname),
            maps:put(Hostname, Address, Map)
        end, #{}, Hostnames
    ).

-spec add_hosts_to_inet(#{string() := inet:ip6_address()}) -> ok.
add_hosts_to_inet(HostnameToIp) ->
    inet_db:set_lookup([file, dns]),
    maps:foreach(fun(Hostname, IP) ->
        inet_db:add_host(IP, [Hostname])
    end, HostnameToIp).

% ------------------------------------------------------------------------------

% See https://fly.io/docs/reference/private-networking/#fly-internal-addresses

% AAAA record of an app instance.
% Returns a hostent record, eg.:
% {hostent,"5683929b651208.vm.braidnet-ams.internal",[], inet6,16, [{64938,0,59229,2683,41599,21029,11428,2}]}}
-spec fly_ns_vm(string()) -> inet:hostent().
fly_ns_vm(AppId) ->
    Name = AppId ++ ".vm." ++ ?app_name ++ ".internal",
    {ok, Res} = inet_res:getbyname(Name, aaaa),
    Res.

% Comma-separated ids of app instances. Ids are used as hostnames by default.
% Returns a hostent record, eg.:
% {hostent,"vms.braidnet-ams.internal",[],txt,1, [["5683929b651208 ams,e28650eeb01e68 ams"]]}
-spec fly_ns_vms() -> inet:hostent().
fly_ns_vms() ->
    Name = "vms." ++ ?app_name ++ ".internal",
    {ok, Res} = inet_res:getbyname(Name, txt),
    Res.

% App instances in all regions.
% Returns a hostent record, eg.:
% {hostent,"global.braidnet-ams.internal",[],inet6,16, [{64938,0,59229,2683,321,5189,43787,2}, {64938,0,59229,2683,41599,21029,11428,2}]}
-spec fly_ns_global() -> inet:hostent().
fly_ns_global() ->
    Name = "global." ++ ?app_name ++ ".internal",
    {ok, Res} = inet_res:getbyname(Name, aaaa),
    Res.
