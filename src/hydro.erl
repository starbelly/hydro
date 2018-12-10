%%% @doc module hydro provides a refreshing interface to hydro_api
%%% @end.
-module(hydro).

-export([rand/1, rand_uniform/1, dice/0]).

-export([hash_keygen/0, hash/2, hash/3, hash_init/1, hash_init/2, hash_update/2,
         hash_final/1]).

-spec rand(non_neg_integer()) -> binary().
rand(N) when N >= 0 ->
    hydro_api:random_buf(N).

-spec rand_uniform(non_neg_integer()) -> integer().
rand_uniform(N) when N >= 0 ->
    hydro_api:random_uniform(N).

%%% @doc
%%% Generates a key suitable for working with the generic hashing functions
%%% @end
-spec hash_keygen() -> binary().
hash_keygen() -> 
    hydro_api:hash_keygen().

%% @doc
%% The hash4 function returns a computed fixed-length finger print (hash)
%% using the supplied context, message, key, and size. Size must be between 32 and 65535.
%% @end
-spec hash(binary(), binary()) -> {ok, binary()} | {error,term()}.
hash(Context, Msg) when is_binary(Msg)
                         andalso  is_binary(Msg) ->
    hydro_api:hash_hash(Msg, Context).

%% @doc
%% The hash4 function returns a computed fixed-length finger print (hash)
%% using the supplied context, message, key, and size. Size must be between 32 and 65535.
%% @end
-spec hash(binary(), binary(), binary()) -> {ok, binary()} | {error,term()}.
hash(Context, Msg, Key) when is_binary(Msg)
                              andalso is_binary(Msg)
                              andalso  is_binary(Key) ->
    hydro_api:hash_hash(Msg, Context, Key).

%% @doc
%% The hash_init/2 initializes state of `Context' context with no key for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(binary()) -> {ok, reference()} | {error, term()}.
hash_init(Context) when is_binary(Context) ->
    hydro_api:hash_init(Context).

%% @doc
%% The hash_init/2 initializes state with a context of `Context', size `Size', and key `Key' for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(binary(), binary()) -> {ok, reference()} | {error, term()}.
hash_init(Context, Key) when is_binary(Context) 
                        andalso is_binary(Key) ->
    hydro_api:hash_init(Context, Key).

%% @doc
%% The hash_update/2 updates the referenced state with the supplied message.
%% @end
-spec hash_update(reference(), binary()) -> {ok, reference()} | {error, term()}.
hash_update(State, Msg) when is_reference(State)
                        andalso is_binary(Msg) ->
    hydro_api:hash_update(State, Msg).

%% @doc
%% The hash_final/2 functions returns a complete hash given a reference to a
%% hash state and an output size.
%% @end
-spec hash_final(reference()) -> {ok, binary()} | {error, term()}.
hash_final(State) when is_reference(State) -> 
    hydro_api:hash_final(State).

-spec dice() -> integer().
dice() -> 
    hydro_api:random_u32().
