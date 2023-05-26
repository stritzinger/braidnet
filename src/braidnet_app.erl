%%%-------------------------------------------------------------------
%% @doc braidnet public API
%% @end
%%%-------------------------------------------------------------------

-module(braidnet_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    {ok, HostName} = application:get_env(hostname),
    Dispatch = cowboy_router:compile([
        {"localhost", [
            {"/braidnode", braidnet_braidnode_api, []},
            % only for development:
            {"/api/[:method]", braidnet_braid_rest_api, []}
        ]},
        {HostName,[
            {"/api/[:method]", braidnet_braid_rest_api, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(example, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    braidnet_sup:start_link().

stop(_State) -> kraft:stop().


%% internal functions
