-module(braidnet_rest_SUITE).

-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").


-compile([export_all, nowarn_export_all]).

%--- API -----------------------------------------------------------------------

all() ->
    [
        F
        ||
        {F, 1} <- ?MODULE:module_info(exports),
        lists:suffix("_test", atom_to_list(F))
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(braidnet),
    application:set_env(braid, scheme, "http"),
    application:set_env(braid, braidnet_domain, "localhost"),
    application:set_env(braid, port, 8080),
    application:set_env(braid, braidnet_access_token, <<"dummy">>),
    Dir = ?config(data_dir, Config),
    BraidFile = filename:join([Dir, "braidnet.test.config"]),
    [{config_file, BraidFile} | Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(_, Config) ->
    BraidFile = ?config(config_file, Config),
    ct:print("~p",[BraidFile]),
    ?assertMatch([{<<"localhost">>, {204, _}}], braid_rest:launch(BraidFile)),
    Config.

end_per_testcase(_, Config) ->
    BraidFile = ?config(config_file, Config),
    ?assertMatch([{<<"localhost">>, {204, _}}], braid_rest:destroy(BraidFile)).

%--- Tests ---------------------------------------------------------------------

list_test(Config) ->
    BraidFile = ?config(config_file, Config),
    ?assertMatch([{<<"localhost">>, {200, [
        #{
            <<"id">> := _,
            <<"image">> := _,
            <<"name">> := _,
            <<"status">> := _
        },
        #{
            <<"id">> := _,
            <<"image">> := _,
            <<"name">> := _,
            <<"status">> := _
        }
    ]}}], braid_rest:list(BraidFile)),
    ok.

% logs_test(Config) ->
%     BraidFile = ?config(config_file, Config),
%     ?assertMatch([{_, <<"ok">>}], braid_rest:logs("tgt", "id")),
%     ok.
