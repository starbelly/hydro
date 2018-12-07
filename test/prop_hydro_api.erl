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

prop_hash_keygen() ->
  ?FORALL({}, {},
  begin
    is_binary(hydro_api:hash_keygen())
  end).

prop_hash_hash() ->
  ?FORALL({Size, Msg, Context, Key}, {range(24,64), binary(), binary(8),
                                      binary(32)},
  begin
    {ok, Hash} = hydro_api:hash_hash(Size, Msg, Context, Key),
    true = is_binary(Hash)
  end).

prop_hash_hash_keyless() ->
  ?FORALL({Size, Msg, Context}, {range(24,64), binary(), binary(8)},
  begin
    {ok, Hash} = hydro_api:hash_hash(Size, Msg, Context),
    true = is_binary(Hash)
  end).

prop_hash_init() ->
  ?FORALL({Size, Context, Key}, {range(32,65535), binary(8),
                                      binary(32)},
  begin
    {ok, State} = hydro_api:hash_init(Size, Context, Key),
    true = is_reference(State)
  end).

prop_hash_init_keyless() ->
  ?FORALL({Size, Context}, {range(32,65535), binary(8)},
  begin
    {ok, State} = hydro_api:hash_init(Size, Context),
    true = is_reference(State)
  end).

prop_hash_update() ->
    ?FORALL({Size, Context, Key, Msg}, {range(32, 65535), binary(8),
                                        binary(32), binary()},
            begin
                {ok, Ref} = hydro_api:hash_init(Size, Context, Key),
                true = is_reference(Ref),
                {ok, true} = hydro_api:hash_update(Ref, Msg),
                true
            end).

prop_hash_final() ->
    ?FORALL({Size, Context, Msg1, Msg2}, {range(32, 65535), binary(8),
                                      binary(), binary()},
            begin
                {ok, Ref} = hydro_api:hash_init(Size, Context),
                true = is_reference(Ref),
                {ok, true} = hydro_api:hash_update(Ref, Msg1),
                {ok, true} = hydro_api:hash_update(Ref, Msg2),
                {ok, Hash} = hydro_api:hash_final(Size, Ref),
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


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
