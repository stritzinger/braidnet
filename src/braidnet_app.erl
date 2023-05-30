%%%-------------------------------------------------------------------
%% @doc braidnet public API
%% @end
%%%-------------------------------------------------------------------

-module(braidnet_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Routes = case application:get_env(hostname) of
        {ok, Hostname} -> routes(Hostname);
        undefined -> routes(local)
    end,
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(example, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    braidnet_sup:start_link().

stop(_State) -> kraft:stop().

%% internal functions

routes(local) ->
    [
        {'_', [
            {"/braidnode", braidnet_braidnode_api, []},
            {"/api/[:method]", braidnet_braid_rest_api, []}
        ]}
    ];
routes(Hostname) ->
    [
        {"localhost", [
            {"/braidnode", braidnet_braidnode_api, []}
        ]},
        {Hostname, [
            {"/api/[:method]", braidnet_braid_rest_api, []}
        ]}
    ].
