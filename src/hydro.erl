%%% @doc module hydro provides a refreshing interface to hydro_api
%%% @end.
-module(hydro).

-export([rand/1, rand_uniform/1, dice/0]).

-export([hash_keygen/0, hash/3, hash/4, hash_init/2, hash_init/3, hash_update/2, hash_final/2]).

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
-spec hash(binary(), binary(), integer()) -> {ok, binary()} | {error,term()}.
hash(Context, Msg, Size) when is_binary(Msg)
                         andalso  is_binary(Msg)
                         andalso is_integer(Size)  ->
    hydro_api:hash_hash(Size, Msg, Context).

%% @doc
%% The hash4 function returns a computed fixed-length finger print (hash)
%% using the supplied context, message, key, and size. Size must be between 32 and 65535.
%% @end
-spec hash(binary(), binary(), integer(), binary()) -> {ok, binary()} | {error,term()}.
hash(Context, Msg, Size, Key) when is_binary(Msg)
                              andalso is_binary(Msg)
                              andalso is_integer(Size)
                              andalso  is_binary(Key) ->
    hydro_api:hash_hash(Size, Msg, Context, Key).

%% @doc
%% The hash_init/2 initializes state of `Context' context and size `Size' with no key for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(binary(), integer()) -> {ok, reference()} | {error, term()}.
hash_init(Context, Size) when is_binary(Context)
                         andalso is_integer(Size) ->
    hydro_api:hash_init(Size, Context).

%% @doc
%% The hash_init/2 initializes state with a context of `Context', size `Size', and key `Key' for a multi-part hash
%% operation. Updates to the state may be perfomed using returned reference and hash_update/2
%% @end
-spec hash_init(binary(), integer(), binary()) -> {ok, reference()} | {error, term()}.
hash_init(Context, Size, Key) when is_binary(Context) 
                              andalso is_integer(Size)
                              andalso is_binary(Key) ->
    hydro_api:hash_init(Size, Context, Key).

%% @doc
%% The hash_update/2 updates the referenced state with the supplied message.
%% @end
-spec hash_update(reference(), binary()) -> ok | {error, term()}.
hash_update(State, Msg) when is_reference(State)
                        andalso is_binary(Msg) ->
    hydro_api:hash_update(State, Msg).

%% @doc
%% The hash_final/2 functions returns a complete hash given a reference to a
%% hash state and an output size.
%% @end
-spec hash_final(reference(), integer()) -> {ok, binary()} | {error, term()}.
hash_final(State, Size) when is_reference(State) 
                        andalso is_integer(Size) ->
    hydro_api:hash_final(Size, State).

-spec dice() -> integer().
dice() -> 
    hydro_api:random_u32().
