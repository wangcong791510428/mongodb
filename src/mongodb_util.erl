%%%-------------------------------------------------------------------
%%% @author wangcong13
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 一月 2019 上午10:09
%%%-------------------------------------------------------------------
-module(mongodb_util).
-author("wangcong13").
-include ("mongo_protocol.hrl").
%% API
-export([random_nonce/1,
         encode_name/1,
         parse_server_responce/1,
         get_value/2,
         hi/3,
         hmac/2,
         xorKeys/3,
         pw_hash/2,
         pw_key/3,
         at/3,
         query_parse/1,
         read_preference/1
        ]).

random_nonce(TextLength) ->
    ByteLength = trunc(TextLength / 4 * 3),
    RandBytes = crypto:strong_rand_bytes(ByteLength),
    base64:encode(RandBytes).

encode_name(Name) ->
    Comma = re:replace(Name, <<"=">>, <<"=3D">>, [{return, binary}]),
    re:replace(Comma, <<",">>, <<"=2C">>, [{return, binary}]).

parse_server_responce(Responce) ->
    ParamList = binary:split(Responce, <<",">>, [global]),
    lists:map(
        fun(Param) ->
            [K, V] = binary:split(Param, <<"=">>),
            {K, V}
        end, ParamList).

get_value(Key, List) -> get_value(Key, List, undefined).

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        false -> Default
    end.

hi(Password, Salt, Iterations) ->
    {ok, Key} = pbkdf2:pbkdf2(sha, Password, Salt, Iterations, 20),
    Key.

hmac(One, Two) -> crypto:hmac(sha, One, Two).

xorKeys(<<>>, _, Res) -> Res;
xorKeys(<<FA, RestA/binary>>, <<FB, RestB/binary>>, Res) ->
    xorKeys(RestA, RestB, <<Res/binary, <<(FA bxor FB)>>/binary>>).

%%pw_hash(Username, Password) ->
%%    bson:utf8(binary_to_hexstr(crypto:hash(md5, [Username, <<":mongo:">>, Password]))).
%%-----------------------------------------------------
%%-spec pw_key (nonce(), username(), password()) -> bson:utf8().
pw_key (Nonce, Username, Password) ->
    bson:utf8 (binary_to_hexstr (erlang:md5 ([Nonce, Username, pw_hash (Username, Password)]))).
%%-spec pw_hash (username(), password()) -> bson:utf8().
pw_hash (Username, Password) ->
    bson:utf8 (binary_to_hexstr (erlang:md5 ([Username, <<":mongo:">>, Password]))).

binary_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

at(Label, Document, Default) ->
    case bson:at(Label, Document) of
        null ->
            Default;
        Value ->
            Value
    end.

query_parse(Sel) ->
    case erlang:tuple_to_list(Sel) of
        [] ->
            ['filter', Sel];
        SelData ->
            data_shaping(SelData, [])
    end.

data_shaping([], Acc) ->
    Acc;

data_shaping([Key, Value|Tail] = Data, Acc) ->
    case Key of
        '$orderby' ->
            Tail1 = Tail,
            Acc1 = [sort, Value| Acc];
        '$query' ->
            Tail1 = Tail,
            Acc1 = ['filter', Value| Acc];
        '$self_query' ->
            Tail1 = Tail,
            Acc1 = ['filter', Value| Acc];
        _ ->
            Tail1 = [],
            Acc1 = ['filter', list_to_tuple(Data)]
    end,
    data_shaping(Tail1, Acc1).

read_preference(SOK) ->
    case SOK of
        true ->
            <<"secondaryPreferred">>;
        _ ->
            <<"primaryPreferred">>
    end.