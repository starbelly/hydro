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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
