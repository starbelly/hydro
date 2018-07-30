Hydro [![Hex Version](https://img.shields.io/hexpm/v/hydro.svg)](https://hex.pm/packages/hydro) [![Gitlab-CI](https://gitlab.com/starbelly/hydro/badges/master/pipeline.svg)](https://gitlab.com/starbelly/hydro/commits/master) [![Travis-CI](https://travis-ci.org/starbelly/hydro.svg?branch=master)](https://travis-ci.org/starbelly/hydro) [![coverage report](https://gitlab.com/starbelly/hydro/badges/master/coverage.svg)](https://gitlab.com/starbelly/hydro/commits/master) [![License](https://img.shields.io/badge/License-MIT-blue.svg)]()
============

Libhydrogen bindings for Erlang

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

## Reference

 - [libhydrogen](https://www.libhydrogen.org/doc/)
