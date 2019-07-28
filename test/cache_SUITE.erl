%%
%%   Copyright 2015 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(cache_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% cache primitives
%%%
%%%----------------------------------------------------------------------------   

lifecycle(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:drop(Cache).

i(_Config) ->
   {ok, Cache} = cache:start_link([]),
   Spec = cache:i(Cache),
   {heap, _} = lists:keyfind(heap, 1, Spec),
   {expire, _} = lists:keyfind(expire, 1, Spec),
   {size, _} = lists:keyfind(size, 1, Spec),
   {memory, _} = lists:keyfind(memory, 1, Spec),
   ok = cache:drop(Cache).

heap(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   [{key, val}] = ets:lookup(cache:heap(Cache, 1), key),
   ok = cache:drop(Cache).

heap_recover_failure(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   cache:heap(Cache, 10000),
   [{key, val}] = ets:lookup(cache:heap(Cache, 1), key),
   ok = cache:drop(Cache).

purge(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:put(Cache, key, val),
   val = cache:get(Cache, key),
   cache:purge(Cache),
   undefined = cache:get(Cache, key),
   ok = cache:drop(Cache).

evict_ttl(_Config) ->
   {ok, Cache} = cache:start_link([{n, 10}, {ttl, 10}]),
   ok = cache:put(Cache, key, val, 3),
   timer:sleep(1200),
   val = cache:lookup(Cache, key),
   timer:sleep(2200),
   undefined = cache:lookup(Cache, key),
   ok = cache:drop(Cache).

evict_no_ttl(_Config) ->
   {ok, Cache} = cache:start_link([{n, 3}, {ttl, 3}]),
   ok = cache:put(Cache, key, val),
   timer:sleep(1200),
   val = cache:lookup(Cache, key),
   timer:sleep(2200),
   undefined = cache:lookup(Cache, key),
   ok = cache:drop(Cache).

evict_ooc(_Config) ->
   {ok, Cache} = cache:start_link([{n, 10}, {size, 20}, {check, 1}]),
   ok = cache:put(Cache, key1, val),
   ok = cache:put(Cache, key2, val),
   [2 | _] = cache:i(Cache, size),
   timer:sleep(1200),
   [0, 2 | _] = cache:i(Cache, size),
   ok = cache:drop(Cache).

%%%----------------------------------------------------------------------------   
%%%
%%% cache basic i/o
%%%
%%%----------------------------------------------------------------------------   

put(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   [{key, val}] = ets:lookup(cache:heap(Cache, 1), key),
   ok = cache:drop(Cache).

put_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:put_(Cache, key, val),
   val = cache:get(Cache, key),
   ok  = cache:drop(Cache).


get(_Config) ->
   {ok, Cache} = cache:start_link([{n, 10}, {ttl, 10}]),
   ok  = cache:put(Cache, key1, val),
   val = cache:get(Cache, key1),
   ok  = cache:put(Cache, key2, val, 5),
   val = cache:get(Cache, key2),
   undefined = cache:get(Cache, unknown),
   ok = cache:drop(Cache).
      
lookup(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:put(Cache, key, val),
   val = cache:lookup(Cache, key),
   undefined = cache:lookup(Cache, unknown),
   ok = cache:drop(Cache).

has(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true  = cache:has(Cache, key),
   false = cache:has(Cache, unknown),
   ok = cache:drop(Cache).

ttl(_Config) ->
   {ok, Cache} = cache:start_link([{n,10}, {ttl, 60}]),
   ok = cache:put(Cache, key1, val),
   true = cache:ttl(Cache, key1) > 55,
   ok = cache:put(Cache, key2, val, 10),
   true = cache:ttl(Cache, key2) > 9,
   undefined = cache:ttl(Cache, unknown),
   ok = cache:drop(Cache).

remove(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true = cache:has(Cache, key),
   ok = cache:remove(Cache, key),   
   false= cache:has(Cache, key),
   ok = cache:drop(Cache).
   
remove_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true = cache:has(Cache, key),
   ok = cache:remove_(Cache, key),   
   false= cache:has(Cache, key),
   ok = cache:drop(Cache).

apply(_Config) ->
   {ok, Cache} = cache:start_link([]),
   val = cache:apply(Cache, key, fun(undefined) -> val end),
   val = cache:get(Cache, key),
   lav = cache:apply(Cache, key, fun(val) -> lav end),
   lav = cache:get(Cache, key),
   ok = cache:drop(Cache).   

apply_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   cache:apply_(Cache, key, fun(undefined) -> val end),
   val = cache:get(Cache, key),
   cache:apply_(Cache, key, fun(val) -> lav end),
   lav = cache:get(Cache, key),
   ok = cache:drop(Cache).   

%%%----------------------------------------------------------------------------   
%%%
%%% cache extended i/o
%%%
%%%----------------------------------------------------------------------------   

acc(_Config) ->
   {ok, Cache} = cache:start_link([]),
   undefined = cache:acc(Cache, key, 1),
   1  = cache:acc(Cache, key, 10),
   11 = cache:acc(Cache, key, 1),
   ok = cache:drop(Cache).

acc_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   cache:acc_(Cache, key, 1),
   cache:acc_(Cache, key, 10),
   11 = cache:get(Cache, key),
   ok = cache:drop(Cache).

set(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:set(Cache, key, val),
   val = cache:get(Cache, key),
   ok  = cache:drop(Cache).
      
add(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:add(Cache, key, val),
   {error, conflict}  = cache:add(Cache, key, val),
   ok  = cache:drop(Cache).

add_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   cache:add_(Cache, key, val),
   cache:add_(Cache, key, non),
   val = cache:get(Cache, key),
   ok  = cache:drop(Cache).

replace(_Config) ->
   {ok, Cache} = cache:start_link([]),
   {error, not_found} = cache:replace(Cache, key, val),
   ok  = cache:set(Cache, key, val),
   ok  = cache:replace(Cache, key, val),
   ok  = cache:drop(Cache).

replace_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   cache:replace_(Cache, key, non),
   undefined = cache:get(Cache, key),
   ok  = cache:set(Cache, key, val),
   cache:replace_(Cache, key, foo),
   foo = cache:get(Cache, key),
   ok  = cache:drop(Cache).

append(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:append(Cache, key, a),
   [a] = cache:get(Cache, key),
   ok  = cache:append(Cache, key, b),
   [a, b] = cache:get(Cache, key),
   ok  = cache:append(Cache, key, c),
   [a, b, c] = cache:get(Cache, key),
   ok  = cache:drop(Cache).

append_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:append_(Cache, key, a),
   ok  = cache:append_(Cache, key, b),
   ok  = cache:append_(Cache, key, c),
   [a, b, c] = cache:get(Cache, key),
   ok  = cache:drop(Cache).

prepend(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:prepend(Cache, key, a),
   [a] = cache:get(Cache, key),
   ok  = cache:prepend(Cache, key, b),
   [b, a] = cache:get(Cache, key),
   ok  = cache:prepend(Cache, key, c),
   [c, b, a] = cache:get(Cache, key),
   ok  = cache:drop(Cache).

prepend_(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:prepend_(Cache, key, a),
   ok  = cache:prepend_(Cache, key, b),
   ok  = cache:prepend_(Cache, key, c),
   [c, b, a] = cache:get(Cache, key),
   ok  = cache:drop(Cache).

delete(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true = cache:has(Cache, key),
   ok = cache:delete(Cache, key),   
   false= cache:has(Cache, key),
   ok = cache:drop(Cache).


