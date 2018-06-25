-module(prop_helium_api).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_random_buf() ->
  ?FORALL({I},{non_neg_integer()},
  begin
    is_binary(helium_api:random_buf(I))
  end).

prop_random_buf_neg_int_fail() ->
  ?FORALL({I},{neg_integer()},
  begin
    try
        helium_api:random_buf(I),
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
