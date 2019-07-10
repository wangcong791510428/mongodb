%%%-------------------------------------------------------------------
%%% @author wangcong13
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 一月 2019 上午10:36
%%%-------------------------------------------------------------------
-module(mongo_auth).
-author("wangcong13").
-include ("mongo_protocol.hrl").
-define(GS2_HEADER, <<"n,,">>).  %%自定义
-define(RANDOM_LENGTH, 24). %%自定义

%-define(DEBUG(F, A), io:format(F++"\n", A)).
-define(DEBUG(_F, _A), "").

%% API
-export([compose_second_message/5,
         generate_proof/2,
         compose_first_message/2,
         generate_sig/2,
         mongodb_old_auth/2,
         auth_first_step/2
        ]).

%%Old mongodb version auth
mongodb_old_auth (Username, Password) ->
    Nonce = bson:at (nonce, mongo:command ({getnonce, 1})),
    try mongo:command ({authenticate, 1, user, Username, nonce, Nonce, key, mongodb_util:pw_key (Nonce, Username, Password)})
    of _ -> true
    catch error:{bad_command, _} -> false end.

%%New mongodb version auth (大于2.7)
auth_first_step (Username, Password) ->
    RandomBString = mongodb_util:random_nonce(?RANDOM_LENGTH),
    FirstMessage = compose_first_message(Username, RandomBString),
    Message = <<?GS2_HEADER/binary, FirstMessage/binary>>,
    Data = {<<"saslStart">>, 1, <<"mechanism">>, <<"SCRAM-SHA-1">>, <<"payload">>, {bin, bin, Message}, <<"autoAuthorize">>, 1},
    Nonce = mongo:command (Data),
    ?DEBUG("~p:auth ~p Nonce = ~p", [?MODULE, ?LINE, Nonce]),
    ConversationId = mongodb_util:at(conversationId, Nonce, {}),
    Payload = bson:at(payload, Nonce),
    auth_second_step(Username, Password, Payload, ConversationId, RandomBString, FirstMessage).

auth_second_step(Login, Password, {bin, bin, Decoded} = _Payload, ConversationId, RandomBString, FirstMessage) ->
    {Signature, ClientFinalMessage} = compose_second_message(Decoded, Login, Password, RandomBString, FirstMessage),
    Data = {<<"saslContinue">>, 1, <<"conversationId">>, ConversationId, <<"payload">>, {bin, bin, ClientFinalMessage}},
    Nonce = mongo:command (Data),
    ?DEBUG("~p:auth_second_step ~p Nonce = ~p", [?MODULE, ?LINE, Nonce]),
    auth_third_step(base64:encode(Signature), Nonce, ConversationId).

auth_third_step(ServerSignature, Response, ConversationId) ->
    {bin, bin, Payload} = bson:at(payload, Response),
    Done = mongodb_util:at(done, Response, false),
    ParamList = mongodb_util:parse_server_responce(Payload),
    ServerSignature = mongodb_util:get_value(<<"v">>, ParamList),
    auth_forth_step(Done, ConversationId).

auth_forth_step(true, _) -> true;
auth_forth_step(false, ConversationId) ->
    Data = {<<"saslContinue">>, 1, <<"conversationId">>, ConversationId, <<"payload">>, {bin, bin, <<>>}},
    Nonce = mongo:command (Data),
    mongodb_util:at(done, Nonce, false).


compose_second_message(Payload, Login, Password, RandomBString, FirstMessage) ->
    ParamList = mongodb_util:parse_server_responce(Payload),
    R = mongodb_util:get_value(<<"r">>, ParamList),
    Nonce = <<<<"r=">>/binary, R/binary>>,
    {0, ?RANDOM_LENGTH} = binary:match(R, [RandomBString], []),
    S = mongodb_util:get_value(<<"s">>, ParamList),
    I = binary_to_integer(mongodb_util:get_value(<<"i">>, ParamList)),
    SaltedPassword = mongodb_util:hi(mongodb_util:pw_hash(Login, Password), base64:decode(S), I),
    ChannelBinding = <<<<"c=">>/binary, (base64:encode(?GS2_HEADER))/binary>>,
    ClientFinalMessageWithoutProof = <<ChannelBinding/binary, <<",">>/binary, Nonce/binary>>,
    AuthMessage = <<FirstMessage/binary, <<",">>/binary, Payload/binary, <<",">>/binary, ClientFinalMessageWithoutProof/binary>>,
    ServerSignature = generate_sig(SaltedPassword, AuthMessage),
    Proof = generate_proof(SaltedPassword, AuthMessage),
    {ServerSignature, <<ClientFinalMessageWithoutProof/binary, <<",">>/binary, Proof/binary>>}.

generate_proof(SaltedPassword, AuthMessage) ->
    ClientKey = mongodb_util:hmac(SaltedPassword, <<"Client Key">>),
    StoredKey = crypto:hash(sha, ClientKey),
    Signature = mongodb_util:hmac(StoredKey, AuthMessage),
    ClientProof = mongodb_util:xorKeys(ClientKey, Signature, <<>>),
    <<<<"p=">>/binary, (base64:encode(ClientProof))/binary>>.

generate_sig(SaltedPassword, AuthMessage) ->
    ServerKey = mongodb_util:hmac(SaltedPassword, "Server Key"),
    mongodb_util:hmac(ServerKey, AuthMessage).

compose_first_message(Login, RandomBString) ->
    UserName = <<<<"n=">>/binary, (mongodb_util:encode_name(Login))/binary>>,
    Nonce = <<<<"r=">>/binary, RandomBString/binary>>,
    <<UserName/binary, <<",">>/binary, Nonce/binary>>.
