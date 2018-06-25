%%% @doc module helium provides a refreshing interface to helium_api
%%% @end.
-module(helium).

-export([rand/1]).

-spec rand(non_neg_integer()) -> binary().
rand(N) when N >= 0 ->
    helium_api:random_buf(N).


