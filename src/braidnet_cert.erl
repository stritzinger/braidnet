-module(braidnet_cert).

-export([
    get_cert_file/1,
    get_private_key_file/1,
    get_ca_file/0,
    delete_braidnode_certfiles/1,
    new_braidnode_cert/1,
    get_stritzinger_ca_file/0
]).

get_cert_file(CID) ->
    File = cert_file_path(CID),
    case filelib:is_file(File) of
        true -> File;
        false -> throw({not_found, File})
    end.

get_private_key_file(CID) ->
    File = key_file_path(CID),
    case filelib:is_file(File) of
        true -> File;
        false -> throw({not_found, File})
    end.

get_ca_file() ->
    File = ca_file_path(),
    case filelib:is_file(File) of
        true -> File;
        false -> throw({not_found, File})
    end.

get_stritzinger_ca_file() ->
    File = stritzinger_ca_file_path(),
    case filelib:is_file(File) of
        true -> File;
        false -> throw({not_found, File})
    end.

delete_braidnode_certfiles(CID) ->
    CertDir = cert_dir_path(CID),
    file:del_dir_r(CertDir).

new_braidnode_cert(CID) ->
    CertDir = cert_dir_path(CID),
    ok = filelib:ensure_path(CertDir),
    %---
    {ok, CertContents} = request_certificate(CID),
    %---
    CertFile = cert_file_path(CID),
    ok = file:write_file(CertFile, CertContents),
    CertFile.

new_private_key(CID) ->
    KeyFile = key_file_path(CID),
    Cmd = "openssl",
    Args = [
        "genrsa",
        "-out", KeyFile,
        "2048"
    ],
    {ok, _} = run_cmd(Cmd, Args),
    ok.

new_signing_request(CID) ->
    ConfigFile = csr_config_file_path(),
    PrivateKeyFile = key_file_path(CID),
    CsrFile = csr_file_path(CID),
    AltName = erlang:binary_to_list(braidnet_cluster:this_nodehost()),
    Cmd = "openssl",
    Args = [
        "req",
        "-new",
        "-config", ConfigFile,
        "-key", PrivateKeyFile,
        "-addext", "subjectAltName=DNS.0:" ++ AltName,
        "-out", CsrFile
    ],
    {ok, _} = run_cmd(Cmd, Args),
    ok.

request_certificate(CID) ->
    ok = new_private_key(CID),
    ok = new_signing_request(CID),
    %---
    {ok, Url} = application:get_env(braidnet, braidcert_url),
    {ok, Key} = application:get_env(braidnet, braidcert_key),
    {ok, Csr} = file:read_file(csr_file_path(CID)),
    Headers = [{"Authorization", "Bearer " ++ Key}],
    Request = {Url ++ CID, Headers, "application/octet-stream", Csr},
    {ok, {_, _, Body}} = httpc:request(put, Request, [], []),
    %---
    file:delete(csr_file_path(CID)),
    %---
    {ok, Body}.

run_cmd(Executable, Args) ->
    Path = os:find_executable(Executable),
    Port = erlang:open_port({spawn_executable, Path},
                            [{args, Args},
                             {line, 1024},
                             use_stdio,
                             binary,
                             exit_status,
                             stderr_to_stdout]),
    case run_cmd_receive(Port, <<"">>, []) of
        {0, Lines} -> {ok, Lines};
        {Error, Lines} -> {{error, Error}, Lines}
    end.

run_cmd_receive(Port, CurrentLine, Lines) ->
    receive
        {Port, {exit_status, Status}} ->
            Lines1 = lists:reverse(Lines),
            {Status, Lines1};
        {Port, {data, {noeol, Data}}} ->
            CurrentLine1 = <<CurrentLine/binary, Data/binary>>,
            run_cmd_receive(Port, CurrentLine1, Lines);
        {Port, {data, {eol, Data}}} ->
            CurrentLine1 = <<CurrentLine/binary, Data/binary>>,
            run_cmd_receive(Port, <<"">>, [CurrentLine1 | Lines])
    end.

csr_config_file_path() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "cfg", "braidnet.cfg"]).

ca_file_path() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "braidcert.CA.pem"]).

stritzinger_ca_file_path() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "stritzinger_grisp_CA.pem"]).

cert_dir_path(CID) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "requests", CID]).

key_file_path(CID) ->
    filename:join(cert_dir_path(CID), "private.key").

csr_file_path(CID) ->
    filename:join(cert_dir_path(CID), "csr.csr").

cert_file_path(CID) ->
    filename:join(cert_dir_path(CID), "cert.pem").
