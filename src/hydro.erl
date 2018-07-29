%%% @doc module hydro provides a refreshing interface to hydro_api
%%% @end.
-module(hydro).

-export([rand/1, rand_uniform/1, dice/0]).

-spec rand(non_neg_integer()) -> binary().
rand(N) when N >= 0 ->
    hydro_api:random_buf(N).

-spec rand_uniform(non_neg_integer()) -> integer().
rand_uniform(N) when N >= 0 ->
    hydro_api:random_uniform(N).

dice() -> 
    hydro_api:random_u32().
