

# Hydro #

__Version:__ Feb 9 2019 20:22:54

![H](assets/logo-xsmall.png) Hydro [![Hex Version](https://img.shields.io/hexpm/v/hydro.svg)](https://hex.pm/packages/hydro)
[![Gitlab-CI](https://gitlab.com/starbelly/hydro/badges/master/pipeline.svg)](https://gitlab.com/starbelly/hydro/commits/master)
[![Travis-CI](https://travis-ci.com/starbelly/hydro.svg?branch=master)](https://travis-ci.com/starbelly/hydro)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)]() ==

[libhydrogen](https://www.libhydrogen.org/doc/) bindings for Erlang

- [About](http://github.com/starbelly/hydro/blob/docs/doc/README.md#about)
- [Installation](http://github.com/starbelly/hydro/blob/docs/doc/README.md#installation)
  * [Rebar3](http://github.com/starbelly/hydro/blob/docs/doc/README.md#rebar3)
  * [Mix](http://github.com/starbelly/hydro/blob/docs/doc/README.md#mix)
- [Usage](http://github.com/starbelly/hydro/blob/docs/doc/README.md#usage)
  * [Hydro](http://github.com/starbelly/hydro/blob/docs/doc/README.md#hydro)
    + [random number and data generation](http://github.com/starbelly/hydro/blob/docs/doc/README.md#random-number-and-data-generation)
      - [rand/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#rand-1)
      - [rand_uniform/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#rand-uniform-1)
      - [rand_int/0](http://github.com/starbelly/hydro/blob/docs/doc/README.md#rand-int-0)
    + [key_gen/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#key-gen-1)
    + [keygen_pair/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#keygen-pair-1)
    + [Generic Hashing](http://github.com/starbelly/hydro/blob/docs/doc/README.md#generic-hashing)
      - [Single part](http://github.com/starbelly/hydro/blob/docs/doc/README.md#single-part)
      - [Multi-part](http://github.com/starbelly/hydro/blob/docs/doc/README.md#multi-part)
    + [Secret boxes](http://github.com/starbelly/hydro/blob/docs/doc/README.md#secret-boxes)
      - [box_seal/2](http://github.com/starbelly/hydro/blob/docs/doc/README.md#box-seal-2)
      - [box_seal/3](http://github.com/starbelly/hydro/blob/docs/doc/README.md#box-seal-3)
      - [box_open/3](http://github.com/starbelly/hydro/blob/docs/doc/README.md#box-open-3)
      - [box_open/4](http://github.com/starbelly/hydro/blob/docs/doc/README.md#box-open-4)
    + [Password hashing and key deirivation](http://github.com/starbelly/hydro/blob/docs/doc/README.md#password-hashing-and-key-deirivation)
      - [password/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#password-1)
      - [password_verify/2](http://github.com/starbelly/hydro/blob/docs/doc/README.md#password-verify-2)
      - [password_upgrade/2](http://github.com/starbelly/hydro/blob/docs/doc/README.md#password-upgrade-2)
    + [Utilities](http://github.com/starbelly/hydro/blob/docs/doc/README.md#utilities)
      - [shuffle/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#shuffle-1)
      - [rand_pick/1](http://github.com/starbelly/hydro/blob/docs/doc/README.md#rand-pick-1)
- [Reference](http://github.com/starbelly/hydro/blob/docs/doc/README.md#reference)


### <a name="About">About</a> ###

Hydro provides Erlang bindings to the libhydrogen cryptographic library ([libhydrogen](https://www.libhydrogen.org/doc/)). Hydro also provides some useful functions as part of the main interface.

Working with hydro is simple in both Erlang and Elixr.

- ___Erlang___

```
1> N = hydro:rand(42).
&#171;115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144&#187;
1> hydro:rand(42).
&#171;196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...&#187;
```

- ___Elixir___

```
iex(1)> n = :hydro.rand(42)
&#171;115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144,110,108,111,101,180...&#187;
```


### <a name="Installation">Installation</a> ###


#### <a name="Rebar3">Rebar3</a> ####

```
{deps, [{hydro, "0.1.0"}]}
```


#### <a name="Mix">Mix</a> ####

```
def deps do
  [{:hydro, "~> 0.1.0"}]
end
```


### <a name="Usage">Usage</a> ###

There are two main interfaces in hydro. The `hydro` module and the `hydro_api` module. The `hydro` module is the primary interface which most users should feel comfortable with. The `hydro_api` module more or less mirrors the libhydrogen api and is intended for advanced usage.

Note that direct access to the nif is not available in this application library.


#### <a name="Hydro">Hydro</a> ####

<h5><a name="random_number_and_data_generation">random number and data generation</a></h5>

<h5><a name="#_rand/1"># rand/1</a></h5>

Generates and returns a random sequence of bytes up to the specified size.

```
1> N = hydro:rand(42).
&#171;115,97,120,157,28,208,118,165,137,95,122,152,195,49,52,
  188,73,136,216,201,77,183,29,144&#187;
1> hydro:rand(42).
&#171;196,216,103,100,59,248,220,7,212,176,11,59,76,54,33,27,
  87,166,157,249,120,120,106,253,233,234,148,136,133,...&#187;
```

<h5><a name="#_rand_uniform/1"># rand_uniform/1</a></h5>

Generates and returns a uniform distributed random value between 0 and the supplied upper
bound (exlusive).

```
1> hydro:rand_uniform(100*100).
1243
2> hydro:rand_uniform(100*100).
901
3> hydro:rand_uniform(100*100).
7966
```

<h5><a name="#_rand_int/0"># rand_int/0</a></h5>

Generates and returns a random value between 0 and 0xffffffff (inclusive).

```
1> hydro:rand_int().
3397059606
2> hydro:rand_int().
2426160804
3> hydro:rand_int().
2140249458
```

<h5><a name="key_gen/1">key_gen/1</a></h5>

```
 hydro:keygen(hash)
```

<h5><a name="keygen_pair/1">keygen_pair/1</a></h5>

<h5><a name="Generic_Hashing">Generic Hashing</a></h5>

Note that a context is an 8 byte binary.

<h5><a name="#_Single_part"># Single part</a></h5>

Without a key

```
1> hydro:hash(&#171;"context0"&#187;, &#171;"msg"&#187;, 64).
{ok,&#171;231,51,76,246,39,178,123,195,254,34,172,216,53,135,
      95,160,94,6,97,118,224,8,66,187,247,150,58,...&#187;}
```

With a key

```
1> hydro:hash(&#171;"context1"&#187;, &#171;"msg"&#187;, 32, hydro:hash_keygen()).
{ok,&#171;55,230,42,62,224,198,89,79,68,155,73,29,38,82,114,
      199,66,128,43,180,27,92,152,135,252,224,145,...&#187;}
```

<h5><a name="#_Multi-part"># Multi-part</a></h5>

Without a key

```
1> {ok, State} = hydro:hash_init(&#171;"context0"&#187;, 42).
{ok,#Ref<0.4176683979.2420768776.167622>}
2> {ok, true} = hydro:hash_update(State, &#171;"MyMsg1"&#187;).
{ok,true}
3> {ok, true} = hydro:hash_update(State, &#171;"MyMsg2"&#187;).
{ok,true}
4> {ok, Hash} = hydro:hash_final(State, 88).
{ok,&#171;44,201,1,123,196,16,201,224,112,217,21,86,149,189,
      159,139,179,108,69,30,129,180,76,56,95,166,17,...&#187;}
```

With key

```
1> {ok, State} = hydro:hash_init(&#171;"context0"&#187;, 64, hydro:hash_keygen()).
{ok,#Ref<0.1264487252.3744858120.160049>}
2> {ok, true} = hydro:hash_update(State, &#171;"MyMsg1"&#187;).
{ok,true}
3> {ok, true} = hydro:hash_update(State, &#171;"MyMsg2"&#187;).
{ok,true}
4> {ok, Hash} = hydro:hash_final(State, 128).
{ok,&#171;41,55,10,38,139,118,239,161,123,224,141,247,74,77,
      123,91,119,28,2,34,69,137,146,203,160,122,81,...&#187;}
```

<h5><a name="Secret_boxes">Secret boxes</a></h5>

<h5><a name="#_box_seal/2"># box_seal/2</a></h5>

<h5><a name="#_box_seal/3"># box_seal/3</a></h5>

<h5><a name="#_box_open/3"># box_open/3</a></h5>

<h5><a name="#_box_open/4"># box_open/4</a></h5>

<h5><a name="Password_hashing_and_key_deirivation">Password hashing and key deirivation</a></h5>

<h5><a name="#_password/1"># password/1</a></h5>

<h5><a name="#_password_verify/2"># password_verify/2</a></h5>

<h5><a name="#_password_upgrade/2"># password_upgrade/2</a></h5>

<h5><a name="Utilities">Utilities</a></h5>

<h5><a name="#_shuffle/1"># shuffle/1</a></h5>


Takes and randomly sorts a list.

1> hydro:shuffle(lists:seq(1,100)).
[7,84,18,10,50,65,94,82,97,92,28,9,86,49,37,20,63,46,66,23, 
64,26,39,16,90,33,15,40,57|...]
2> hydro:shuffle(lists:seq(1,100)).
[46,3,5,17,52,38,4,50,55,21,71,80,51,54,88,30,96,18,44,42, 
87,29,68,79,65,14,34,53,99|...]
3> hydro:shuffle(lists:seq(1,100)).
[81,77,38,63,89,43,79,46,31,28,41,8,68,93,92,76,5,24,29,17, 
23,13,26,27,60,71,34,9,40|...]

<h5><a name="#_rand_pick/1"># rand_pick/1</a></h5>

Takes a list, randomly sorts it, and randomly picks an element from the list.

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


#### <a name="Hydro_API">Hydro API</a> ####

See the edocs and source code for the `hydro_api` documentation.


### <a name="Reference">Reference</a> ###
- [libhydrogen](https://www.libhydrogen.org/doc/)

<script>
// Jump directly to a referenced url given in trailing '[]:...'-notation
function goto(tag) { parent.document.location.href = url(tag); }
function url(tag) { var o=document.getElementById(tag); return o ? o.href : '#'+tag; }
</script>



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/starbelly/hydro/blob/docs/doc/hydro.md" class="module">hydro</a></td></tr>
<tr><td><a href="http://github.com/starbelly/hydro/blob/docs/doc/hydro_api.md" class="module">hydro_api</a></td></tr></table>

