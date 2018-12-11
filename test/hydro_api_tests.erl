-module(hydro_api_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% The expected results below were obtained directly from libhydrogen in C

generichash_keyless_test() -> 
    Expected =
    <<"5112339920a88d57c19c9ed8ab44e0a07b5ece2a7bba461d7878b4d7ee3df43c">>,
    {ok, H} = hydro_api:hash_hash(<<"There is plenty for the both of us, may the best Dwarf win.">>, <<"libtests">>),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).

generichash_multi_keyless_test() -> 
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

generichash_test() -> 
    Expected =
    <<"87309f1d3f269b023f25c7255aaed441e37fb81c0404b31a43061cae64d59874">>,
    Key =  <<"Nobody tosses a dwarf and foobar">>,
    {ok, H} = hydro_api:hash_hash(<<"There is plenty for the both of us, may the best Dwarf win.">>, <<"libtests">>,
                                 Key),
    Hex = hydro_api:bin2hex(H),
    ?assertEqual(Hex, Expected).

generichash_multi_test() -> 
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
