-module(braidnet_cluster).

-behaviour(gen_server).

-export([
    this_nodehost/0,
    this_nodename/0
]).

% gen_server API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-include_lib("kernel/include/inet.hrl").

%-------------------------------------------------------------------------------
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

%-------------------------------------------------------------------------------
start_link() ->
    case os:getenv("FLY_APP_NAME") of
        false ->
            % No need for this gen_server on your local machine.
            ignore;
        _ ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])
    end.

init(_) ->
    Machines = fly_machines(),
    Hosts = host_to_ip_map(Machines),
    % ---
    % https://web.archive.org/web/20150607063315/http://osdir.com/ml/lang.erlang.general/2004-04/msg00155.html
    % https://twitter.com/st_deerl/status/489739674412679168
    inet_db:set_lookup([file, dns]),
    maps:foreach(fun(M, IP) ->
        inet_db:add_host(IP, [M])
    end, Hosts),
    % ---
    ping_fly_hosts(maps:keys(Hosts)),
    logger:notice("Connected Braidnet nodes: ~p~n", [nodes()]),
    add_hostname_to_hosts_file(),
    {ok, []}.

handle_call(Request, _From, State) ->
    logger:notice("Unexpected call: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    logger:notice("Unexpected cast: ~p~n", [Request]),
    {noreply, State}.

handle_info(Request, State) ->
    logger:notice("Unexpected info: ~p~n", [Request]),
    {noreply, State}.

%-------------------------------------------------------------------------------
-spec fly_machines() -> [string()].
fly_machines() ->
    #hostent{h_addr_list = MachineList} = fly_ns_machines(),
    MachinesWithRegions = string:split(lists:flatten(MachineList), ","),
    lists:map(fun(M) ->
        [Machine, _Region] = string:split(M, " "),
        Machine
    end, MachinesWithRegions).

host_to_ip_map(Machines) ->
    lists:foldl(fun(Machine, Acc) ->
        IP = fly_ip(Machine),
        Hostname = fly_long_hostname(Machine),
        maps:put(Hostname, IP, Acc)
    end, #{}, Machines).

-spec fly_ip(string()) -> inet:ip_address().
fly_ip(Machine) ->
    #hostent{h_addr_list = [Address]} = fly_ns_machine(Machine),
    Address.

ping_fly_hosts(Hosts) ->
    lists:foreach(fun(Host) ->
        HostBin = erlang:list_to_binary(Host),
        Name = this_nodename(),
        Node = erlang:binary_to_atom(<<Name/binary, "@", HostBin/binary>>),
        logger:notice("~p pings ~p: ~p~n", [node(), Node, net_adm:ping(Node)])
    end, Hosts).

fly_app_name() ->
    os:getenv("FLY_APP_NAME").

fly_long_hostname(Machine) ->
    string:join([Machine, "vm", fly_app_name(), "internal"], ".").

add_hostname_to_hosts_file() ->
    % This is needed to get the remote shell to work. For now.
    Comment = "# Remote shell support:",
    Entry = io_lib:format("~n~s~n127.0.0.1 ~s~n", [Comment, this_nodehost()]),
    file:write_file("/etc/hosts", Entry, [append]).

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
