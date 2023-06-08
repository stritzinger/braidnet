%%%-------------------------------------------------------------------
%% @doc orchestrator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(braidnet_container_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
        #{id => braidnet_container,
          start => {braidnet_container, start_link, []},
          type => worker,
          restart => transient}
    ],
    {ok, {SupFlags, ChildSpecs}}.
