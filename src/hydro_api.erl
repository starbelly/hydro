%%% @doc module hydro_api provides bindings to libhydrogen for Erlang
%%% @end.
-module(hydro_api).

-define(APPNAME, hydro).
-define(LIBNAME, hydro_nif).

-on_load(init/0).

-export([random_buf/1, random_u32/0, random_uniform/1]).

-spec random_buf(non_neg_integer()) -> binary().
random_buf(N) when N >= 0 ->
    hydro_random_buf(N).

-spec random_uniform(non_neg_integer()) -> integer().
random_uniform(N) when N >= 0 ->
    hydro_random_uniform(N).

-spec random_u32() -> integer().
random_u32() ->
    hydro_random_u32().

%%% @private
init() ->
  SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

hydro_random_buf(_requestedsize) -> 
    erlang:nif_error(nif_not_loaded).

hydro_random_uniform(_UpperBound) -> 
    erlang:nif_error(nif_not_loaded).

hydro_random_u32() -> 
    erlang:nif_error(nif_not_loaded).
