-module(braidnet_cluster).

-export([
    start/0,
    this_nodehost/0,
    this_nodename/0,
    host_environment/0
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/inet.hrl").

%-------------------------------------------------------------------------------
-spec start() -> ok | ignore.
start() ->
    case host_environment() of
        fly ->
            Hosts = update_dns(),
            ping_braidnets(Hosts);
        _ ->
            ?LOG_NOTICE("Not on Fly.io ... Not checking the cluster."),
            ignore
    end.

% Returns the host part of the long name of this node, as a binary.
-spec this_nodehost() -> binary().
this_nodehost() ->
    Node = erlang:atom_to_binary(node()),
    [_, Host] = binary:split(Node, <<"@">>),
    Host.

% Returns the name part of the long name of this node, as a binary.
-spec this_nodename() -> binary().
this_nodename() ->
    Node = erlang:atom_to_binary(node()),
    [Name, _] = binary:split(Node, <<"@">>),
    Name.

% Checks what kind of host environment this Braidnet is running on.
-spec host_environment() -> atom().
host_environment() ->
    case os:getenv("FLY_APP_NAME") of
        false ->
            localhost;
        _ ->
            fly
    end.

%-------------------------------------------------------------------------------
-spec fly_machines() -> [string()].
fly_machines() ->
    #hostent{h_addr_list = MachineList} = fly_ns_machines(),
    MachinesWithRegions = string:split(lists:flatten(MachineList), ",", all),
    lists:map(fun(M) ->
        [Machine, _Region] = string:split(M, " "),
        Machine
    end, MachinesWithRegions).

-spec host_to_ip_map([string()]) -> #{string() := inet:ip6_address()}.
host_to_ip_map(Machines) ->
    lists:foldl(fun(Machine, Acc) ->
        IP = fly_ip(Machine),
        maps:put(Machine, IP, Acc)
    end, #{}, Machines).

-spec fly_ip(string()) -> inet:ip6_address().
fly_ip(Machine) ->
    #hostent{h_addr_list = [Address]} = fly_ns_machine(Machine),
    Address.

-spec update_dns() -> [string()].
update_dns() ->
    Hostnames = fly_machines(),
    HostIpMap = host_to_ip_map(Hostnames),
    %--
    % https://web.archive.org/web/20150607063315/http://osdir.com/ml/lang.erlang.general/2004-04/msg00155.html
    % https://twitter.com/st_deerl/status/489739674412679168
    inet_db:set_lookup([file, dns]),
    maps:foreach(fun(M, IP) -> inet_db:add_host(IP, [M]) end, HostIpMap),
    %--
    Hostnames.

-spec ping_braidnets([string()]) -> ok.
ping_braidnets(Hosts) ->
    OtherHosts = lists:delete(net_adm:localhost(), Hosts),
    ?LOG_DEBUG("Trying to ping hosts: ~p...",[OtherHosts]),
    lists:foreach(fun(Host) ->
        HostBin = erlang:list_to_binary(Host),
        Name = this_nodename(),
        Node = erlang:binary_to_atom(<<Name/binary, "@", HostBin/binary>>),
        ?LOG_NOTICE("~p pings ~p: ~p~n", [node(), Node, net_adm:ping(Node)])
    end, OtherHosts).

-spec fly_app_name() -> string().
fly_app_name() ->
    os:getenv("FLY_APP_NAME").

%-------------------------------------------------------------------------------
% Fly.io private network queries
% See https://fly.io/docs/reference/private-networking/#fly-internal-addresses

% Comma-separated ids+regions of the machines.
% Returns a hostent record, eg.:
% {hostent,"vms.braidnet-ams.internal",[],txt,1,
%  [["5683929b651208 ams,e28650eeb01e68 ams"]]}
-spec fly_ns_machines() -> inet:hostent().
fly_ns_machines() ->
    Name = "vms." ++ fly_app_name() ++ ".internal",
    {ok, Res} = inet_res:getbyname(Name, txt),
    Res.

% AAAA record of a machine.
% Returns a hostent record, eg.:
% {hostent,"5683929b651208.vm.braidnet-ams.internal",[], inet6,16,
%  [{64938,0,59229,2683,41599,21029,11428,2}]}}
-spec fly_ns_machine(string()) -> inet:hostent().
fly_ns_machine(Machine) ->
    Name = Machine ++ ".vm." ++ fly_app_name() ++ ".internal",
    {ok, Res} = inet_res:getbyname(Name, aaaa),
    Res.
