Hydro [![Hex Version](https://img.shields.io/hexpm/v/hydro.svg)](https://hex.pm/packages/hydro) [![Gitlab-CI](https://gitlab.com/starbelly/hydro/badges/master/pipeline.svg)](https://gitlab.com/starbelly/hydro/commits/master) [![Travis-CI](https://travis-ci.com/starbelly/hydro.svg?branch=master)](https://travis-ci.com/starbelly/hydro) [![License](https://img.shields.io/badge/License-MIT-blue.svg)]()
============

Libhydrogen bindings for Erlang

* [Installation](#installation)
    * [Rebar3](#rebar3)
    * [Mix](#mix)
* [Usage](#usage)
    * [Hydro](#hydro)
        * [dice/0](#dice0)
        * [generic hashing](#generic_hashing)
        * [rand/1](#rand1)
        * [rand_uniform/1](#rand_uniform1)
    * [Hydro API](#hydro-api-1)
        * [hash_keygen/0](#hash_keygen)
        * [random_u32/0](#random_u320)
        * [random_buf/1](#random_buf1)
        * [random_uniform/1](#random_uniform1)
* [Reference](#reference)

## About

hydro provides Erlang bindings to the Hydrogen Crypto Library ([libhydrogen](https://www.libhydrogen.org/doc/)).

Working with hydro is simple in both Erlang and Elixr.

- ***Erlang***
```erlang
1> N = hydro:rand(42).
<<115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144>>
1> hydro:rand(42).
<<196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...>>
```

- ***Elixir***
```elixir
iex(1)> n = :hydro.rand(42)
<<115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144,110,108,111,101,180...>>
```

## Installation

### Rebar3

```erlang
{deps, [{hydro, "0.1.0"}]}
```

### Mix

```elixir
def deps do
  [{:hydro, "~> 0.1.0"}]
end
```

## Usage

### Hydro

#### dice/0

Generates and returns a random value between 0 and 0xffffffff (inclusive).

```erlang
1> hydro:dice().
3397059606
2> hydro:dice().
2426160804
3> hydro:dice().
2140249458
```

#### shuffle/1 
 Takes and randomly sorts a list. 

#### rand_pick/1
  Takes a list, randomly sorts it, and randomly picks an element from the list.

#### Generic Hashing

Note that a context is an 8 byte binary. 

##### Single part

Without a key

```erlang
1> hydro:hash(<<"context0">>, <<"msg">>, 64).
{ok,<<231,51,76,246,39,178,123,195,254,34,172,216,53,135,
      95,160,94,6,97,118,224,8,66,187,247,150,58,...>>}
```

With a key

```erlang
1> hydro:hash(<<"context1">>, <<"msg">>, 32, hydro:hash_keygen()).
{ok,<<55,230,42,62,224,198,89,79,68,155,73,29,38,82,114,
      199,66,128,43,180,27,92,152,135,252,224,145,...>>}
```

##### Multi-part 

Without a key

```erlang
1> {ok, State} = hydro:hash_init(<<"context0">>, 42).
{ok,#Ref<0.4176683979.2420768776.167622>}
2> {ok, true} = hydro:hash_update(State, <<"MyMsg1">>).
{ok,true}
3> {ok, true} = hydro:hash_update(State, <<"MyMsg2">>).
{ok,true}
4> {ok, Hash} = hydro:hash_final(State, 88).
{ok,<<44,201,1,123,196,16,201,224,112,217,21,86,149,189,
      159,139,179,108,69,30,129,180,76,56,95,166,17,...>>}
```

With key

```erlang
1> {ok, State} = hydro:hash_init(<<"context0">>, 64, hydro:hash_keygen()).
{ok,#Ref<0.1264487252.3744858120.160049>}
2> {ok, true} = hydro:hash_update(State, <<"MyMsg1">>).
{ok,true}
3> {ok, true} = hydro:hash_update(State, <<"MyMsg2">>).
{ok,true}
4> {ok, Hash} = hydro:hash_final(State, 128).
{ok,<<41,55,10,38,139,118,239,161,123,224,141,247,74,77,
      123,91,119,28,2,34,69,137,146,203,160,122,81,...>>}
```

#### rand/1

Generates and returns a random sequence of bytes up to the specified size. 

```erlang
1> N = hydro:rand(42).
<<115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144>>
1> hydro:rand(42).
<<196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...>>
```

#### rand_uniform/1

Generates and returns a uniform distributed random value between 0 and the supplied upper
bound (exlusive).

```erlang
1> hydro:rand_uniform(100*100).
1243
2> hydro:rand_uniform(100*100).
901
3> hydro:rand_uniform(100*100).
7966
```

### Hydro API

 For advanced usage there is the hydro api.

#### hash_keygen/0
Create a secret key suitable for use with hash*

```erlang
1> hydro_api:hash_keygen().
<<196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...>>
```

#### random_u32/0

Generates and returns a random value between 0 and 0xffffffff (inclusive).

```erlang
1> hydro_api:random_u32().
537265334
2> hydro_api:random_u32().
623392245
3> hydro_api:random_u32().
533755626
4>
```

#### random_buf/1

```erlang
1> hydro_api:random_buf(42).
<<196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...>>
2> hydro_api:random_buf(42).
<<161,240,252,190,45,94,112,230,59,93,7,151,230,50,244,
  105,208,28,6,148,116,101,117,107,226,0,43,9,89,...>>
3> hydro_api:random_buf(42).
<<208,189,65,169,124,15,251,118,0,61,33,12,188,100,91,129,
  95,166,210,85,9,196,247,251,70,74,61,27,19,...>>
```

Generates and returns a random sequence of bytes up to the specified size. 

#### random_uniform/1

Generates and returns a uniform distributed random value between 0 and the supplied upper
bound (exlusive).

```erlang
5> hydro_api:random_uniform(100*100).
5260
6> hydro_api:random_uniform(100*100).
6564
7> hydro_api:random_uniform(100*100).
9529
```

## Reference

 - [libhydrogen](https://www.libhydrogen.org/doc/)
