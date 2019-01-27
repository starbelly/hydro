-module(hydro_api_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% The expected results below were obtained directly from libhydrogen in C

api_test_() ->
   api_harness_gen(1000).

api_harness_gen(N) ->
    List =  {setup, fun setup/0, fun cleanup/1,
             {inparallel, [ fun generichash/0,
                            fun generichash_multi/0,
                            fun generichash_keyless/0,
                            fun generichash_multi_keyless/0]}},
    {generator,
     fun () ->
             if N > 0 ->
                    [ List | api_harness_gen(N-1)];
                true ->
                    []
             end
     end}.

setup() -> ok.

cleanup(_Eh) -> ok.

generichash_keyless() ->
    Expected =
    <<"5112339920a88d57c19c9ed8ab44e0a07b5ece2a7bba461d7878b4d7ee3df43c">>,
    {ok, H} = hydro_api:hash_hash(<<"There is plenty for the both of us, may the best Dwarf win.">>, <<"libtests">>),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).

generichash_multi_keyless() ->
    Str1 = <<"There is plenty for the both of us, ">>,
    Str2 = <<"may the best Dwarf win.">>,
    Expected =
    <<"5112339920a88d57c19c9ed8ab44e0a07b5ece2a7bba461d7878b4d7ee3df43c">>,
    {ok, State} = hydro_api:hash_init(<<"libtests">>),
    {ok, State1} =  hydro_api:hash_update(State, Str1),
    {ok, State2} = hydro_api:hash_update(State1, Str2),
    {ok, H} = hydro_api:hash_final(State2),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).

generichash() ->
    Expected =
    <<"87309f1d3f269b023f25c7255aaed441e37fb81c0404b31a43061cae64d59874">>,
    Key =  <<"Nobody tosses a dwarf and foobar">>,
    {ok, H} = hydro_api:hash_hash(<<"There is plenty for the both of us, may the best Dwarf win.">>, <<"libtests">>,
                                 Key),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).

generichash_multi() ->
    Str1 = <<"There is plenty for the both of us, ">>,
    Str2 = <<"may the best Dwarf win.">>,
    Key = <<"Nobody tosses a dwarf and foobar">>,
    Expected =
    <<"87309f1d3f269b023f25c7255aaed441e37fb81c0404b31a43061cae64d59874">>,
    {ok, State} = hydro_api:hash_init(<<"libtests">>, Key),
    {ok, State1} =  hydro_api:hash_update(State, Str1),
    {ok, State2} = hydro_api:hash_update(State1, Str2),
    {ok, H} = hydro_api:hash_final(State2),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).
