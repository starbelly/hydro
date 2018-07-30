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
