-module(braidnet_jsonrpc).

% API
-export([call/2]).
-export([call/3]).
-export([notification/1]).
-export([notification/2]).
-export([result/2]).
-export([error/2]).
-export([error/3]).
-export([error/4]).
-export([decode/1]).

-define(
    is_method(Method),
    (is_atom(Method) orelse is_binary(Method))
).
-define(
    is_message(Method),
    (is_atom(Method) orelse is_binary(Method))
).
-define(
    is_params(Params),
    (is_map(Params) orelse is_list(Params) orelse is_tuple(Params))
).
-define(
    is_internal_code(Code),
    Code >= -32099, Code =< -32000; Code == -32700; Code == -32600;
    Code == -32601; Code == -32602; Code == -32603
).

%--- API -----------------------------------------------------------------------

call(Method, Id) when ?is_method(Method) ->
    encode(#{method => Method, id => Id}).

call(Method, Params, Id) when ?is_method(Method), ?is_params(Params) ->
    encode(#{method => Method, params => Params, id => Id}).

notification(Method) when ?is_method(Method) ->
    encode(#{method => Method}).

notification(Method, Params) when ?is_method(Method), ?is_params(Params) ->
    encode(#{method => Method, params => Params}).

result(Result, Id) ->
    encode(#{result => Result, id => Id}).

error(Atom, Id) -> error_reply(Atom, Id).

error(Code, Message, Id) -> error(Code, Message, undefined, Id).

error(Code, _Message, _Data, _Id) when ?is_internal_code(Code) ->
    error({reserved_error_code, Code});
error(_Code, Message, _Data, _Id) when not ?is_message(Message) ->
    error({invalid_message, Message});
error(Code, Message, undefined, Id) ->
    Error = #{code => Code, message => Message},
    encode(#{error => Error, id => Id});
error(Code, Message, Data, Id) ->
    Error = #{code => Code, message => Message, data => Data},
    encode(#{error => Error, id => Id}).


decode(JSON) ->
    try
        case jiffy:decode(JSON, [return_maps]) of
            #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Decoded ->
                case get(<<"params">>, Decoded) of
                    Params when
                      is_map(Params); is_list(Params); Params == undefined ->
                        case id(Decoded) of
                            null -> {notification, Method, Params};
                            Id   -> {call, Method, Params, Id}
                        end;
                    _Else ->
                        decode_error(invalid_params, id(Decoded))
                end;
            #{<<"jsonrpc">> := <<"2.0">>, <<"result">> := Result, <<"id">> := Id} ->
                {result, Result, Id};
            #{<<"jsonrpc">> := <<"2.0">>, <<"error">> := #{<<"code">> := Code, <<"message">> := Message} = Error, <<"id">> := Id} ->
                {error, Code, Message, get(<<"data">>, Error), Id};
            #{<<"jsonrpc">> := <<"2.0">>} = Decoded ->
                decode_error(method_not_found, id(Decoded));
            Decoded ->
                decode_error(invalid_request, id(Decoded))
        end
    catch
        error:{Pos, Reason}:_ ->
            decode_error({parse_error, Pos, Reason}, null)
    end.

%--- Internal -----------------------------------------------------------------

decode_error({parse_error, _, _} = Reason, Id) ->
    {error, Reason, error_reply(parse_error, Id)};
decode_error(Reason, Id) ->
    {error, Reason, error_reply(Reason, Id)}.

encode(Message) ->
    jiffy:encode(maps:merge(Message, #{jsonrpc => <<"2.0">>})).

error_reply(parse_error, Id) ->
    encode_error(-32700, <<"Parse error">>, Id);
error_reply(invalid_request, Id) ->
    encode_error(-32600, <<"Invalid Request">>, Id);
error_reply(method_not_found, Id) ->
    encode_error(-32601, <<"Method not found">>, Id);
error_reply(invalid_params, Id) ->
    encode_error(-32602, <<"Invalid params">>, Id);
error_reply(internal_error, Id) ->
    encode_error(-32603, <<"Internal error">>, Id).

encode_error(Code, Message, Id) ->
    Error = #{code => Code, message => Message},
    jiffy:encode(#{jsonrpc => <<"2.0">>, error => Error, id => Id}).

id(Map) -> maps:get(<<"id">>, Map, null).

get(Key, Map) -> maps:get(Key, Map, undefined).
