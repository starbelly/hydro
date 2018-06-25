%%% @doc module helium_api provides bindings to libhydrogen for Erlang
%%% @end.
-module(helium_api).

-define(APPNAME, helium).
-define(LIBNAME, helium_nif).

-on_load(init/0).

-export([random_buf/1]).

-spec random_buf(non_neg_integer()) -> binary().
random_buf(N) when N >= 0 ->
    hydro_random_buf(N).

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

hydro_random_buf(_RequestedSize)                                          -> erlang:nif_error(nif_not_loaded).
