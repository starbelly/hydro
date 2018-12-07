%%% @doc module hydro_api provides bindings to libhydrogen for Erlang
%%% @end.
-module(hydro_api).

-define(APPNAME, hydro).
-define(LIBNAME, hydro_nif).

-on_load(init/0).

% rnd
-export([random_buf/1, random_u32/0, random_uniform/1]).

% generic  hashing
-export([
         hash_hash/3, 
         hash_hash/4, 
         hash_init/2,
         hash_init/3, 
         hash_update/2, 
         hash_final/2, 
         hash_keygen/0
        ]). 

-spec hash_keygen() -> binary().
hash_keygen() -> 
    hydro_hash_keygen().

-spec hash_hash(integer(), binary(), binary()) -> {ok, binary()} | {error, term()}.
hash_hash(Size, Msg, Context) -> 
    hydro_hash_hash(Size, Msg, Context, <<"">>).

-spec hash_hash(integer(), binary(), binary(), binary()) -> {ok, binary()} | {error, term()}.
hash_hash(Size, Msg, Context, Key) -> 
    hydro_hash_hash(Size, Msg, Context, Key).

-spec hash_init(integer(), binary()) -> {ok, reference()} | {error, term()}.
hash_init(Size, Context) -> 
    hydro_hash_init(Size, Context, <<"">>).

-spec hash_init(integer(), binary(), binary()) -> {ok, reference()} | {error, term()}.
hash_init(Size, Context, Key) -> 
    hydro_hash_init(Size, Context, Key).

-spec hash_update(reference(), binary()) -> {ok, boolean()} | {error, term()}.
hash_update(State, Msg) -> 
    hydro_hash_update(State, Msg).

-spec hash_final(integer(), reference()) -> {ok, binary()} | {error, term()}.
hash_final(Size, State) -> 
    hydro_hash_final(Size, State).

-spec random_buf(non_neg_integer()) -> binary().
random_buf(N) when N >= 0 ->
    hydro_random_buf(N).

-spec random_uniform(non_neg_integer()) -> integer().
random_uniform(N) when N >= 0 ->
    hydro_random_uniform(N).

-spec random_u32() -> integer().
random_u32() ->
    hydro_random_u32().

%%% @private
init() ->
  SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

hydro_hash_keygen() -> 
    erlang:nif_error(nif_not_loaded).

hydro_hash_hash(_Size, _Msg, _Ctx, _Key) -> 
    erlang:nif_error(nif_not_loaded).

hydro_hash_init(_Size, _Ctx, _Key) -> 
    erlang:nif_error(nif_not_loaded).

hydro_hash_update(_State, _Msg) -> 
    erlang:nif_error(nif_not_loaded).

hydro_hash_final(_Size, _State) -> 
    erlang:nif_error(nif_not_loaded).

hydro_random_buf(_requestedsize) -> 
    erlang:nif_error(nif_not_loaded).

hydro_random_uniform(_UpperBound) -> 
    erlang:nif_error(nif_not_loaded).

hydro_random_u32() -> 
    erlang:nif_error(nif_not_loaded).
