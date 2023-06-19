-module(braidnet_container_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

start_link(Name, CID, Opts) ->
    supervisor:start_link(?MODULE, [Name, CID, Opts]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Name, CID, Opts]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{id => braidnet_container,
          start => {braidnet_container, start_link, [Name, CID, Opts]},
          type => worker,
          restart => transient}
    ],
    {ok, {SupFlags, ChildSpecs}}.
