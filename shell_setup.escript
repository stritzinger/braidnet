#!/usr/bin/env escript
-export([main/1]).

main([]) ->
    Headers = [{"Authorization", "Bearer " ++ "dummy"}],
    Request = {"http://localhost:8081/ca", Headers},
    {ok, {_, _, Body}} = httpc:request(get, Request, [], []),
    {ok, Cwd} = file:get_cwd(),
    SavePath = filename:join([Cwd, "certs", "braidcert.CA.pem"]),
    file:write_file(SavePath, Body).
