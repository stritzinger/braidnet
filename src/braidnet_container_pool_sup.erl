-module(braidnet_container_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},
    ChildSpecs = [
        #{id => braidnet_container_sup,
          start => {braidnet_container_sup, start_link, []},
          type => supervisor,
          restart => temporary}
    ],
    {ok, {SupFlags, ChildSpecs}}.