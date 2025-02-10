%%%-------------------------------------------------------------------
%% @doc braidnet public API
%% @end
%%%-------------------------------------------------------------------

-module(braidnet_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    braidnet_cluster:start(),
    %---
    {ok, Port} = application:get_env(braidnet, api_port),
    HostEnv = braidnet_cluster:host_environment(),
    Dispatch = cowboy_router:compile(routes(HostEnv)),
    {ok, _} = cowboy:start_clear(example, [{port, list_to_integer(Port)}], #{
        env => #{dispatch => Dispatch}
    }),
    %---
    ?LOG_DEBUG("Starting the docker daemon..."),
    os:cmd("/usr/local/bin/dockerd-entrypoint.sh &"),
    %---
    braidnet_sup:start_link().

stop(_State) -> ok.

%% internal functions ----------------------------------------------------------

% Note: requests that come in on fly.io, use an internal IP as hostname.
routes(localhost) ->
    [
        {'_', [
            {"/braidnode", braidnet_braidnode_api, []},
            {"/api/[:method]", braidnet_braid_rest_api, []},
            {"/hc", braidnet_healthcheck, []}
        ]}
    ];
routes(_) ->
    [
        {"localhost", [
            {"/braidnode", braidnet_braidnode_api, []}
        ]},
        {'_', [
            {"/api/[:method]", braidnet_braid_rest_api, []},
            {"/hc", braidnet_healthcheck, []}
        ]}
    ].
