-module(prop_hydro_api).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_random_uniform() ->
  ?FORALL({I},{non_neg_integer()},
  begin
    is_integer(hydro_api:random_uniform(I))
  end).

prop_random_u32() ->
  ?FORALL({}, {},
  begin
    is_integer(hydro_api:random_u32())
  end).

prop_random_ratchet() ->
  ?FORALL({}, {},
  begin
    ok = hydro_api:random_ratchet(),
    true
  end).

prop_hash_keygen() ->
  ?FORALL({}, {},
  begin
    is_binary(hydro_api:hash_keygen())
  end).

prop_hash_hash() ->
  ?FORALL({Msg, Context, Key}, {binary(), binary(8),
                                      binary(32)},
  begin
    {ok, Hash} = hydro_api:hash_hash(Msg, Context, Key),
    true = is_binary(Hash)
  end).

prop_hash_hash_keyless() ->
  ?FORALL({Msg, Context}, {binary(), binary(8)},
  begin
    {ok, Hash} = hydro_api:hash_hash(Msg, Context),
    true = is_binary(Hash)
  end).

prop_hash_init() ->
  ?FORALL({Context, Key}, {binary(8),
                                      binary(32)},
  begin
    {ok, State} = hydro_api:hash_init(Context, Key),
    true = is_reference(State)
  end).

prop_hash_init_keyless() ->
  ?FORALL({Context}, {binary(8)},
  begin
    {ok, State} = hydro_api:hash_init(Context),
    true = is_reference(State)
  end).

prop_hash_update() ->
    ?FORALL({Context, Key, Msg}, {binary(8),
                                        binary(32), binary()},
            begin
                {ok, Ref} = hydro_api:hash_init(Context, Key),
                true = is_reference(Ref),
                {ok, _Ref1} = hydro_api:hash_update(Ref, Msg),
                true
            end).

prop_hash_final() ->
    ?FORALL({Context, Msg1, Msg2}, {binary(8),
                                      binary(), binary()},
            begin
                {ok, Ref} = hydro_api:hash_init(Context),
                true = is_reference(Ref),
                {ok, Ref1} = hydro_api:hash_update(Ref, Msg1),
                {ok, Ref2} = hydro_api:hash_update(Ref1, Msg2),
                {ok, Hash} = hydro_api:hash_final(Ref2),
                is_binary(Hash)
            end).

prop_random_buf() ->
  ?FORALL({I},{non_neg_integer()},
  begin
    is_binary(hydro_api:random_buf(I))
  end).

prop_random_buf_neg_int_fail() ->
  ?FORALL({I},{neg_integer()},
  begin
    try
        hydro_api:random_buf(I),
        false
    catch
        error:badarg          -> true;
        error:function_clause -> true
    end    
  end).

prop_kdf_derive_from_key() ->
  ?FORALL({Ctx, Master, Id, Size}, {binary(8), binary(32), range(16, 65535),
                                    range(16, 65535)},
  begin
    K = hydro_api:kdf_keygen(),
    true = is_binary(K),
    {ok, SubKey} = hydro_api:kdf_derive_from_key(Ctx, Master, Id, Size),
    true = is_binary(SubKey)
  end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
