%%% @doc module hydro provides a refreshing interface to hydro_api
%%% @end.
-module(hydro).

-export([rand/1]).

-spec rand(non_neg_integer()) -> binary().
rand(N) when N >= 0 ->
    hydro_api:random_buf(N).


