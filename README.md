# Cache

Library implements segmented in-memory cache.

[![Build Status](https://github.com/fogfish/cache/workflows/build/badge.svg)](https://github.com/fogfish/cache/actions/)
[![Coverage Status](https://coveralls.io/repos/github/fogfish/cache/badge.svg?branch=master)](https://coveralls.io/github/fogfish/cache?branch=master) 
[![Hex.pm](https://img.shields.io/hexpm/v/cache.svg)](https://hex.pm/packages/cache)
[![Hex Downloads](https://img.shields.io/hexpm/dt/cache.svg)](https://hex.pm/packages/cache)

## Inspiration

Cache uses N disposable ETS tables instead of single one. The cache applies eviction and quota
policies at segment level. The oldest ETS table is destroyed and new one is created when 
quota or TTL criteria are exceeded. This approach outperforms the traditional timestamp indexing techniques.

The write operation always uses youngest segment. The read operation lookup key from youngest to oldest table until it is found same time key is moved to youngest segment to prolong TTL. If none of ETS table contains key then cache-miss occurs. 

The downside is inability to assign precise TTL per single cache entry. TTL is always approximated to nearest segment. (e.g. cache with 60 sec TTL and 10 segments has 6 sec accuracy on TTL) 

## Key features

* Key/value interface to read/write cached entities
* Naive transform interface (accumulators, lists, binaries) to modify entities in-place
* Check-and-store of put behavior
* Supports asynchronous I/O to cache buckets
* Sharding of cache bucket


## Getting started

The latest version of the library is available at its `master` branch. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in contribution guidelines.

The stable library release is available via hex packages, add the library as dependency to `rebar.config`

```erlang
{deps, [
   cache
]}.
```


### Usage

The library exposes public primary interface through exports of module [`cache.erl`](src/cache.erl).
An experimental features are available through interface extensions. Please note that further releases of library would promote experimental features to [primary interface](src/cache.erl).
* [`sharded_cache.erl`](src/cache_shards.erl)

Build library and run the development console to evaluate key features

```
make && make run
```

### spawn and configure

Use `cache:start_link(...)` to spawn an new cache instance. It supports a configuration using property lists:
* `type` - a type of ETS table to used as segment, default is `set`. See `ets:new/2` documentation for supported values. 
* `n` - number of cache segments, default is 10.
* `ttl` - time to live of cached items in seconds, default is 600 seconds. It is recommended to use value multiple to `n`. The oldest cache segment is evicted every `ttl / n` seconds. 
* `size` - number of items to store in cache. It is recommended to use value multiple to `n`, each cache segment takes about `size / n` items. The size policy is applied only to youngest segment.
* `memory` - rough number of bytes available for cache items. Each cache segment is allowed to take about `memory / n` bytes. Note: policy enforcement accounts Erlang word size.
* `policy` - cache eviction policy, default is `lru`, supported values are Least Recently Used `lru`, Most Recently Used `mru`.
* `check` - time in seconds to enforce cache policy. The default behavior enforces policy every `ttl / n` seconds. This timeout helps to optimize size/memory policy enforcement at high throughput system. The timeout is disabled by default. 
* `stats` - cache statistics handler either function/2 or `{M, F}` struct.
* `heir` - the ownership of ETS segment is given away to the process during segment eviction. See `ets:give_away/3` for details.  


### key/value interface

The library implements traditional key/value interface through `put`, `get` and `remove` functions. The function `get` prolongs ttl of the item, use `lookup` to keep ttl untouched.

```erlang
application:start(cache).
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).

ok  = cache:put(my_cache, <<"my key">>, <<"my value">>).
Val = cache:get(my_cache, <<"my key">>).
```


### asynchronous i/o

The library provides synchronous and asynchronous implementation of same functions. The asynchronous variant of function is annotated with `_` suffix. E.g. `get(...)` is a synchronous cache lookup operation (the process is blocked until cache returns); `get_(...)` is an asynchronous variant that delivers result of execution to mailbox.

```erlang
application:start(cache).
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).

Ref = cache:get_(my_cache, <<"my key">>).
receive {Ref, Val} -> Val end.
```

### transform element

The library allows to read-and-modify (modify in-place) cached element. You can `apply` any function over cached elements and returns the result of the function. The apply acts a transformer with three possible outcomes:
* `undefined` (e.g. `fun(_) -> undefined end`) - no action is taken, old cache value remains;
* unchanged value (e.g. `fun(X) -> X end`) - no action is taken, old cache value remains;
* new value (e.g. `fun(X) -> <<"x", X/binary>> end`) - the value in cache is replaced with the result of the function.

```erlang
application:start(cache).
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).

cache:put(my_cache, <<"my key">>, <<"x">>).
cache:apply(my_cache, <<"my key">>, fun(X) -> <<"x", X/binary>> end).
cache:get(my_cache, <<"my key">>).
```

The library implement helper functions to transform elements with `append` or `prepend`.

```erlang
application:start(cache).
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).

cache:put(my_cache, <<"my key">>, <<"b">>).
cache:append(my_cache, <<"my key">>, <<"c">>).
cache:prepend(my_cache, <<"my key">>, <<"a">>).
cache:get(my_cache, <<"my key">>).
```

### accumulator

```erlang
application:start(cache).
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).

cache:acc(my_cache, <<"my key">>, 1).
cache:acc(my_cache, <<"my key">>, 1).
cache:acc(my_cache, <<"my key">>, 1).
```

### check-and-store

The library implements the check-and-store semantic for put operations:
* `add` store key/val only if cache does not already hold data for this key
* `replace` store key/val only if cache does hold data for this key


### configuration via Erlang `sys.config`

The cache instances are configurable via `sys.config`. Theses cache instances are supervised by application supervisor.

```erlang
{cache, [
   {my_cache, [{n, 10}, {ttl, 60}]}
]}
```

### distributed environment

The cache application uses standard Erlang distribution model.
Please node that Erlang distribution uses single tcp/ip connection for message passing between nodes. 
Therefore, frequent read/write of large entries might impact on overall Erlang performance. 


The global cache instance is visible to all Erlang nodes in the cluster.
```erlang
%% at a@example.com
{ok, _} = cache:start_link({global, my_cache}, [{n, 10}, {ttl, 60}]).
Val = cache:get({global, my_cache}, <<"my key">>).

%% at b@example.com
ok  = cache:put({global, my_cache}, <<"my key">>, <<"my value">>).
Val = cache:get({global, my_cache}, <<"my key">>).
```

The local cache instance is accessible for any Erlang nodes in the cluster. 

```erlang
%% a@example.com
{ok, _} = cache:start_link(my_cache, [{n, 10}, {ttl, 60}]).
Val = cache:get(my_cache, <<"my key">>).

%% b@example.com
ok  = cache:put({my_cache, 'a@example.com'}, <<"my key">>, <<"my value">>).
Val = cache:get({my_cache, 'a@example.com'}, <<"my key">>).
```


### sharding

Module `cache_shards` provides simple sharding on top of `cache`. It uses simple `hash(Key) rem NumShards` approach, and keeps `NumShards` in application environment. This feature is still **experimental**, its interface is a subject to change in further releases. 

```erlang
{ok, _} = cache_shards:start_link(my_cache, 8, [{n, 10}, {ttl, 60}]).
ok = cache_shards:put(my_cache, key1, "Hello").
{ok,"Hello"} = cache_shards:get(my_cache, key1).
```

`sharded_cache` uses only small subset of `cache` API. But you can get shard name for your key and then use `cache` directly.
```erlang
{ok, Shard} = cache_shards:get_shard(my_cache, key1)
{ok, my_cache_2}
cache:lookup(Shard, key1).
"Hello"
```


## How to Contribute

The library is Apache 2.0 licensed and accepts contributions via GitHub pull requests.

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Added some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

The development requires [Erlang/OTP](http://www.erlang.org/downloads) version 19.0 or later and essential build tools.


### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>

## Bugs

If you detect a bug, please bring it to our attention via GitHub issues. Please make your report detailed and accurate so that we can identify and replicate the issues you experience:
- specify the configuration of your environment, including which operating system you're using and the versions of your runtime environments
- attach logs, screen shots and/or exceptions if possible
- briefly summarize the steps you took to resolve or reproduce the problem


## Changelog

* 2.3.0 - sharding of cache bucket (single node only)
* 2.0.0 - various changes on asynchronous api, not compatible with version 1.x 
* 1.0.1 - production release

## Contributors

* [Yuri Zhloba](https://github.com/yzh44yzh) 
* [Jose Luis Navarro](https://github.com/artefactop)
* Valentin Micic


## License

Copyright 2014 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

