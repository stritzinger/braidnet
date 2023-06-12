-module(braidnet_healthcheck).
% Provides an API endpoint for Braidnet health checks.
% Used by Fly.io only for now.

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200, Req0),
    {ok, Req, State}.
