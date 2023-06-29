-module(braidnet_container_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

start_link(Name, CID, Opts) ->
    supervisor:start_link(?MODULE, [Name, CID, Opts]).

init([Name, CID, Opts]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5,
                 auto_shutdown => any_significant},
    ChildSpecs = [
        #{id => braidnet_container,
          start => {braidnet_container, start_link, [Name, CID, Opts]},
          type => worker,
          restart => transient,
          significant => true}
    ],
    {ok, {SupFlags, ChildSpecs}}.
