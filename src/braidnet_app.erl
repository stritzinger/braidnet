%%%-------------------------------------------------------------------
%% @doc braidnet public API
%% @end
%%%-------------------------------------------------------------------

-module(braidnet_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_StartType, _StartArgs) ->
    try braidnet_fly_priv_net:discover()
    catch _E:_R:_S -> ?LOG_NOTICE("Could not discover Fly machines.")
    end,

    Port = application:get_env(braidnet, port, 8080),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/braidnode", braidnet_braidnode_api, []},
            {"/api/[:method]", braidnet_braid_rest_api, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(example, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    ?LOG_DEBUG("Starting the docker deamon..."),
    os:cmd("/usr/local/bin/dockerd-entrypoint.sh &"),
    braidnet_sup:start_link().

stop(_State) -> kraft:stop().

%% internal functions

% routes(local) ->
%     [
%         {'_', [
%             {"/braidnode", braidnet_braidnode_api, []},
%             {"/api/[:method]", braidnet_braid_rest_api, []}
%         ]}
%     ];
% routes(Hostname) ->
%     [
%         {"localhost", [
%             {"/braidnode", braidnet_braidnode_api, []}
%         ]},
%         {Hostname, [
%             {"/api/[:method]", braidnet_braid_rest_api, []}
%         ]}
%     ].
