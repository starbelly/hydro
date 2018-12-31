%%% @doc module hydro provides a refreshing interface to hydro_api
%%% @end.
-module(hydro).

-include("hydro.hrl").


-define(KEYGENS,
        #{
          hash => hash_keygen,
          kdf => kdf_keygen,
          secretbox => secretbox_keygen,
          password => pwhash_keygen
         }
       ).

-define(KEYGEN_PAIRS,
        #{
          sign => sign_keygen
         }
       ).

-export([bin2hex/1, rand/1, rand_uniform/1, dice/0, keygen/1, keygen_pair/1]).

-export([hash_keygen/0, hash/2, hash/3, hash_init/1, hash_init/2, hash_update/2,
         hash_final/1]).

-export([box_seal/2, box_open/3, box_seal/3, box_open/4]).

-spec bin2hex(binary()) -> binary().
bin2hex(Bin) ->
    hydro_api:bin2hex(Bin).

-spec box_seal(ctx(), key()) -> 
    {ok, hash(), key()} | {error, term()}.
box_seal(C, M) -> 
    K = hydro_api:secretbox_keygen(),
    case hydro_api:secretbox_encrypt(C, M, K) of 
        {ok, H} -> {ok, H, K};
        {error, _} = Err -> Err
    end.

-spec box_open(ctx(), hash(), key()) -> 
    {ok, binary()} | {error, term()}.
box_open(C, H, K) -> 
    hydro_api:secretbox_decrypt(C, H, K).

-spec box_seal(ctx(), msg(), id()) -> 
    {ok, hash(), key()} | {error, term()}.
box_seal(C, M, I) -> 
    K = hydro_api:secretbox_keygen(),
    case hydro_api:secretbox_encrypt(C, M, I, K) of 
        {ok, H} -> {ok, H, K};
        {error, _} = Err -> Err
    end.

-spec box_open(ctx(), hash(), id(), key()) -> 
    {ok, msg()} | {error, term()}.
box_open(C, H, I, K) -> 
    hydro_api:secretbox_decrypt(C, H, I, K).

-spec rand(non_neg_integer()) -> binary().
rand(N) when N >= 0 ->
    hydro_api:random_buf(N).

-spec rand_uniform(buf_size()) -> non_neg_integer().
rand_uniform(N) when N >= 0 ->
    hydro_api:random_uniform(N).

%%% @doc
%%% Generates a key suitable for working with the generic hashing functions
%%% @end
-spec hash_keygen() -> key().
hash_keygen() -> 
    hydro_api:hash_keygen().

-spec keygen(key_type()) -> key().
keygen(KeyType) when is_atom(KeyType) ->
    case maps:get(KeyType, ?KEYGENS, none) of
        none -> {error, unknown_key_type};
        FunName -> hydro_api:FunName()
    end.

-spec keygen_pair(keypair_type()) -> {ok, key(), key()}.
keygen_pair(KeyPairType) when is_atom(KeyPairType) ->
    case maps:get(KeyPairType, ?KEYGEN_PAIRS, none) of
        none -> {error, unknown_key_type};
        FunName -> hydro_api:FunName()
    end.

%% @doc
%% The hash4 function returns a computed fixed-length finger print (hash)
%% using the supplied context, message, key, and size. Size must be between 32 and 65535.
%% @end
-spec hash(ctx(), msg()) -> {ok, hash()} | {error,term()}.
hash(Context, Msg) when is_binary(Msg)
                         andalso  is_binary(Msg) ->
    hydro_api:hash_hash(Msg, Context).

%% @doc
%% The hash4 function returns a computed fixed-length finger print (hash)
%% using the supplied context, message, key, and size. Size must be between 32 and 65535.
%% @end
-spec hash(ctx(), msg(), key()) -> {ok, hash()} | {error,term()}.
hash(Context, Msg, Key) when is_binary(Msg)
                              andalso is_binary(Msg)
                              andalso  is_binary(Key) ->
    hydro_api:hash_hash(Msg, Context, Key).

%% @doc
%% The hash_init/2 initializes state of `Context' context with no key for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(ctx()) -> {ok, hash_state()} | {error, term()}.
hash_init(Context) when is_binary(Context) ->
    hydro_api:hash_init(Context).

%% @doc
%% The hash_init/2 initializes state with a context of `Context', size `Size', and key `Key' for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(ctx(), key()) -> {ok, hash_state()} | {error, term()}.
hash_init(Context, Key) when is_binary(Context) 
                        andalso is_binary(Key) ->
    hydro_api:hash_init(Context, Key).

%% @doc
%% The hash_update/2 updates the referenced state with the supplied message.
%% @end
-spec hash_update(hash_state(), msg()) -> {ok, hash_state()} | {error, term()}.
hash_update(State, Msg) when is_reference(State)
                        andalso is_binary(Msg) ->
    hydro_api:hash_update(State, Msg).

%% @doc
%% The hash_final/2 functions returns a complete hash given a reference to a
%% hash state and an output size.
%% @end
-spec hash_final(hash_state()) -> {ok, hash()} | {error, term()}.
hash_final(State) when is_reference(State) -> 
    hydro_api:hash_final(State).

-spec dice() -> non_neg_integer().
dice() -> 
    hydro_api:random_u32().
