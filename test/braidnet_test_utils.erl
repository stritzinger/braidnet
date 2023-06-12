-module(braidnet_test_utils).

-export([
    fly_machine_names/0,
    fly_restart_app/0
]).

-spec fly_machine_names() -> [binary()].
fly_machine_names() ->
    Cmd = "flyctl machines list --json -a " ++ fly_app_name(),
    Ret = os:cmd(Cmd),
    MachineMaps = jiffy:decode(Ret, [return_maps]),
    lists:map(fun(M) -> maps:get(<<"id">>, M) end, MachineMaps).

fly_restart_app() ->
    Cmd = "flyctl apps restart " ++ fly_app_name(),
    os:cmd(Cmd).

fly_app_name() ->
    "braidnet-testing".
