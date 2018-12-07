-module(prop_hydro).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_rand() ->
  ?FORALL({I},{non_neg_integer()},
  begin
    is_binary(hydro:rand(I))
  end).

prop_rand_uniform() ->
  ?FORALL({I},{non_neg_integer()},
  begin
    is_integer(hydro:rand_uniform(I))
  end).

prop_dice() ->
  ?FORALL({}, {},
  begin
    is_integer(hydro:dice())
  end).


prop_hash() ->
    ?FORALL({Context, Msg, Key, Size}, {binary(8), non_empty(binary(24)),
                                        non_empty(binary(32)),
                               range(32,65535)},
            begin
                {ok, Bin} = hydro:hash(Context, Msg, Size, Key),
                true = is_binary(Bin)
            end).

prop_hash_multi() ->
    ?FORALL({Context, Key, Msg1, Msg2, Size1, Size2}, {binary(8),
                                                       non_empty(binary(32)), non_empty(binary(32)),
                                      non_empty(binary(24)), range(32,65535), range(32,65535)},
            begin
                {ok, Ref} = hydro:hash_init(Context, Size1, Key),
                true = is_reference(Ref),
                {ok, true} = hydro:hash_update(Ref, Msg1),
                {ok, true} = hydro:hash_update(Ref, Msg2),
                {ok, Hash1} = hydro:hash_final(Ref, Size2),
                true = is_binary(Hash1),
                {ok, Ref1} = hydro:hash_init(Context, Size1, Key),
                true = is_reference(Ref1),
                {ok, true} = hydro:hash_update(Ref1, Msg1),
                {ok, true} = hydro:hash_update(Ref1, Msg2),
                {ok, Hash2} = hydro:hash_final(Ref1, Size2),
                true = is_binary(Hash2),
                equals(Hash1, Hash2)
            end).

prop_hash_multi_keyless() ->
    ?FORALL({Context, Msg1, Msg2, Size1, Size2}, {binary(8), non_empty(binary(32)),
                                      non_empty(binary(24)), range(32,65535), range(32,65535)},
            begin
                {ok, Ref} = hydro:hash_init(Context, Size1),
                true = is_reference(Ref),
                {ok, true} = hydro:hash_update(Ref, Msg1),
                {ok, true} = hydro:hash_update(Ref, Msg2),
                {ok, Hash1} = hydro:hash_final(Ref, Size2),
                true = is_binary(Hash1),
                {ok, Ref1} = hydro:hash_init(Context, Size1),
                true = is_reference(Ref1),
                {ok, true} = hydro:hash_update(Ref1, Msg1),
                {ok, true} = hydro:hash_update(Ref1, Msg2),
                {ok, Hash2} = hydro:hash_final(Ref1, Size2),
                true = is_binary(Hash2),
                equals(Hash1, Hash2)
            end).



%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
