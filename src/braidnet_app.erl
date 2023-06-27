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
    Port = application:get_env(braidnet, port, 8080),
    Hostname = application:get_env(braidnet, hostname, "localhost"),
    Dispatch = cowboy_router:compile(routes(Hostname)),
    {ok, _} = cowboy:start_clear(example, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    %---
    ?LOG_DEBUG("Starting the docker deamon..."),
    os:cmd("/usr/local/bin/dockerd-entrypoint.sh &"),
    %---
    braidnet_sup:start_link().

stop(_State) -> ok.

%% internal functions ----------------------------------------------------------

routes("localhost") ->
    [
        {'_', [
            {"/braidnode", braidnet_braidnode_api, []},
            {"/api/[:method]", braidnet_braid_rest_api, []},
            {"/hc", braidnet_healthcheck, []}
        ]}
    ];
routes(Hostname) ->
    [
        {Hostname, [
            {"/api/[:method]", braidnet_braid_rest_api, []},
            {"/hc", braidnet_healthcheck, []}
        ]},
        {"localhost", [
            {"/braidnode", braidnet_braidnode_api, []}
        ]}
    ].
