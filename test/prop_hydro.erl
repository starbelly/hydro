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
    ?FORALL({Context, Msg, Key}, {binary(8), non_empty(binary(24)),
                                        non_empty(binary(32))},
            begin
                {ok, Bin} = hydro:hash(Context, Msg, Key),
                true = is_binary(Bin)
            end).

prop_hash_multi() ->
    ?FORALL({Context, Key, Msg1, Msg2}, {binary(8), non_empty(binary(32)),
                                         non_empty(binary(32)),
                                         non_empty(binary(24))},
            begin
                {ok, Ref} = hydro:hash_init(Context, Key),
                true = is_reference(Ref),
                {ok, Ref1} = hydro:hash_update(Ref, Msg1),
                {ok, Ref2} = hydro:hash_update(Ref1, Msg2),
                {ok, Hash1} = hydro:hash_final(Ref2),
                true = is_binary(Hash1),
                {ok, Ref3} = hydro:hash_init(Context, Key),
                true = is_reference(Ref1),
                {ok, Ref4} = hydro:hash_update(Ref3, Msg1),
                {ok, Ref5} = hydro:hash_update(Ref4, Msg2),
                {ok, Hash2} = hydro:hash_final(Ref5),
                true = is_binary(Hash2),
                equals(Hash1, Hash2)
            end).

prop_hash_multi_keyless() ->
    ?FORALL({Context, Msg1, Msg2}, {binary(8), non_empty(binary(32)),
                                      non_empty(binary(24))},
            begin
                {ok, Ref} = hydro:hash_init(Context),
                true = is_reference(Ref),
                {ok, Ref1} = hydro:hash_update(Ref, Msg1),
                {ok, Ref2} = hydro:hash_update(Ref1, Msg2),
                {ok, Hash1} = hydro:hash_final(Ref2),
                true = is_binary(Hash1),
                {ok, Ref3} = hydro:hash_init(Context),
                true = is_reference(Ref1),
                {ok, Ref4} = hydro:hash_update(Ref3, Msg1),
                {ok, Ref5} = hydro:hash_update(Ref4, Msg2),
                {ok, Hash2} = hydro:hash_final(Ref5),
                true = is_binary(Hash2),
                equals(Hash1, Hash2)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
