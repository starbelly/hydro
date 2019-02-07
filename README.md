![H](assets/logo-xsmall.png) Hydro [![Hex Version](https://img.shields.io/hexpm/v/hydro.svg)](https://hex.pm/packages/hydro) [![Gitlab-CI](https://gitlab.com/starbelly/hydro/badges/master/pipeline.svg)](https://gitlab.com/starbelly/hydro/commits/master) [![Travis-CI](https://travis-ci.com/starbelly/hydro.svg?branch=master)](https://travis-ci.com/starbelly/hydro) [![License](https://img.shields.io/badge/License-MIT-blue.svg)]()
============


[libhydrogen](https://www.libhydrogen.org/doc/) bindings for Erlang

- [About](#about)
- [Installation](#installation)
  * [Rebar3](#rebar3)
  * [Mix](#mix)
- [Usage](#usage)
  * [Hydro](#hydro)
    + [random number and data generation](#random-number-and-data-generation)
      - [rand/1](#rand-1)
      - [rand_uniform/1](#rand-uniform-1)
      - [rand_int/0](#rand-int-0)
    + [key_gen/1](#key-gen-1)
    + [keygen_pair/1](#keygen-pair-1)
    + [Generic Hashing](#generic-hashing)
      - [Single part](#single-part)
      - [Multi-part](#multi-part)
    + [Secret boxes](#secret-boxes)
      - [box_seal/2](#box-seal-2)
      - [box_seal/3](#box-seal-3)
      - [box_open/3](#box-open-3)
      - [box_open/4](#box-open-4)
    + [Password hashing and key deirivation](#password-hashing-and-key-deirivation)
      - [password/1](#password-1)
      - [password_verify/2](#password-verify-2)
      - [password_upgrade/2](#password-upgrade-2)
    + [Utilities](#utilities)
      - [shuffle/1](#shuffle-1)
      - [rand_pick/1](#rand-pick-1)
- [Reference](#reference)

## About

Hydro provides Erlang bindings to the libhydrogen cryptographic library ([libhydrogen](https://www.libhydrogen.org/doc/)). Hydro also provides some useful functions as part of the main interface. 

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

 There are two main interfaces in hydro. The `hydro` module and the `hydro_api` module. The `hydro` module is the primary interface which most users should feel comfortable with. The `hydro_api` module more or less mirrors the libhydrogen api and is intended for advanced usage. 
 
 Note that direct access to the nif is not available in this application library. 

### Hydro

#### random number and data generation 


##### rand/1

Generates and returns a random sequence of bytes up to the specified size. 

```erlang
1> N = hydro:rand(42).
<<115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144>>
1> hydro:rand(42).
<<196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...>>
```

##### rand_uniform/1

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

##### rand_int/0

Generates and returns a random value between 0 and 0xffffffff (inclusive).

```erlang
1> hydro:rand_int().
3397059606
2> hydro:rand_int().
2426160804
3> hydro:rand_int().
2140249458
```

#### key_gen/1

```erlang
 hydro:keygen(hash)
```

#### keygen_pair/1

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

#### Secret boxes

##### box_seal/2

##### box_seal/3

##### box_open/3

##### box_open/4

#### Password hashing and key deirivation 

##### password/1

##### password_verify/2

##### password_upgrade/2 

#### Utilities


##### shuffle/1 
 Takes and randomly sorts a list. 

```erlang
1> hydro:shuffle(lists:seq(1,100)).
[7,84,18,10,50,65,94,82,97,92,28,9,86,49,37,20,63,46,66,23,
 64,26,39,16,90,33,15,40,57|...]
2> hydro:shuffle(lists:seq(1,100)).
[46,3,5,17,52,38,4,50,55,21,71,80,51,54,88,30,96,18,44,42,
 87,29,68,79,65,14,34,53,99|...]
3> hydro:shuffle(lists:seq(1,100)).
[81,77,38,63,89,43,79,46,31,28,41,8,68,93,92,76,5,24,29,17,
 23,13,26,27,60,71,34,9,40|...]
```

##### rand_pick/1
  Takes a list, randomly sorts it, and randomly picks an element from the list.

```erlang
1> hydro:rand_pick(lists:seq(1,100)).
23
2> hydro:rand_pick(lists:seq(1,100)).
27
3> hydro:rand_pick(lists:seq(1,100)).
93
4> hydro:rand_pick(lists:seq(1,100)).
82
5> hydro:rand_pick(lists:seq(1,100)).
98
6> hydro:rand_pick(lists:seq(1,100)).
13
```

### Hydro API

 See the edocs and source code for the `hydro_api` documentation. 
 
## Reference

 - [libhydrogen](https://www.libhydrogen.org/doc/)
