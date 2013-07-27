%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
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
%%  @description
%%   segmented in-memory cache
%%      * cache memory is split into N segments
%%      * cache applies eviction and quota policies at segment level 
%%        (e.g. whole segments is destroyed at time)
%%      * cache add new items to youngest segment
%%      * cache lookup items from youngest to oldest segment  
%%
%%  @todo
%%   * unit tests (improve coverage)
%%   * cache read/write through handler
%%   * cache atomic update (in-place update)
%%   * memcached protocol 
-module(cache).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("cache.hrl").

-export([
   start_link/2,
   drop/1,
   i/1,
   i/2,
   heap/2,
   put/3, 
   put/4, 
   put_/3, 
   put_/4,
   get/2, 
   lookup/2,
   has/2, 
   ttl/2,
   remove/2, 
   remove_/2
]).

-export_type([cache/0]).

-type(cache()  :: atom() | {atom(), atom()} | {global, atom()} | pid()).
-type(name()   :: atom() | {global, atom()}).
-type(key()    :: any()).
-type(entity() :: any()).
-type(ttl()    :: integer()).

%%
%% start new cache bucket, accepted options:
%%    {policy, lru | mru} - cache eviction policy
%%    {memory, integer()} - cache memory quota in bytes
%%    {size,   integer()} - cache cardinality quota
%%    {n,      integer()} - number of cache segments
%%    {ttl,    integer()} - default time-to-live in seconds
%%    {quota,  integer()} - frequency of quota check in seconds
%%    {stats,  function() | {Mod, Fun}} - cache statistic aggregate functor 
-spec(start_link/2 :: (name(), list()) -> {ok, pid()} | {error, any()}).

start_link(Cache, Opts) ->
   cache_bucket:start_link(Cache, Opts).

%%
%% drop cache
-spec(drop/1 :: (cache()) -> ok).

drop(undefined) ->
   ok;
drop({global, Cache}) ->
   drop(global:whereis_name(Cache));
drop({Cache, Node}) ->
   drop(rpc:call(Node, cache, drop, [Cache]));
drop(Cache)
 when is_atom(Cache) ->
   drop(whereis(Cache));
drop(Cache)
 when is_pid(Cache) ->
   erlang:exit(Cache, shutdown).

%%
%% return cache meta data
%%    {heap,   [integer()]} - cache segments references 
%%    {expire, [integer()]} - cache segments expire times
%%    {size,   [integer()]} - cardinality of cache segments
%%    {memory, [integer()]} - memory occupied by each cache segment
-spec(i/1 :: (cache()) -> list()).
-spec(i/2 :: (cache(), atom()) -> list()).

i(Cache) ->
   gen_server:call(Cache, i).   

i(Cache, Name) ->
   proplists:get_value(Name, i(Cache)).

%%
%% return nth cache segment (e.g. heap(..., 1) returns youngest segment)
-spec(heap/2 :: (cache(), integer()) -> integer() | badarg). 

heap(Cache, N) ->
   gen_server:call(Cache, {heap, N}).


%%
%% synchronous cache put
-spec(put/3  :: (cache(), key(), entity()) -> ok).
-spec(put/4  :: (cache(), key(), entity(), ttl()) -> ok).

put(Cache, Key, Val) ->
   gen_server:call(Cache, {put, Key, Val}, ?DEF_CACHE_TIMEOUT).

put(Cache, Key, Val, TTL) ->
   gen_server:call(Cache, {put, Key, Val, TTL}, ?DEF_CACHE_TIMEOUT).

%%
%% asynchronous cache put
-spec(put_/3 :: (cache(), key(), entity()) -> ok).
-spec(put_/4 :: (cache(), key(), entity(), ttl()) -> ok).

put_(Cache, Key, Val) ->
   gen_server:cast(Cache, {put, Key, Val}).

put_(Cache, Key, Val, TTL) ->
   gen_server:cast(Cache, {put, Key, Val, TTL}).

%%
%% synchronous get cache entry, the operation prolongs entry ttl
-spec(get/2 :: (cache(), key()) -> entity() | undefined).

get(Cache, Key) ->
   gen_server:call(Cache, {get, Key}, ?DEF_CACHE_TIMEOUT).

%%
%% synchronous lookup cache entry, the operation do not prolong entry ttl
-spec(lookup/2 :: (cache(), key()) -> entity() | undefined).

lookup(Cache, Key) ->
   gen_server:call(Cache, {lookup, Key}, ?DEF_CACHE_TIMEOUT).

%%
%% check if cache key exists, 
-spec(has/2 :: (cache(), key()) -> true | false).

has(Cache, Key) ->
   gen_server:call(Cache, {has, Key}, ?DEF_CACHE_TIMEOUT).

%%
%% check entity at cache and return estimated ttl
-spec(ttl/2 :: (cache(), key()) -> ttl() | false).

ttl(Cache, Key) ->
   gen_server:call(Cache, {ttl, Key}, ?DEF_CACHE_TIMEOUT).

%%
%% synchronous remove entry from cache
-spec(remove/2  :: (cache(), key()) -> ok).

remove(Cache, Key) ->
   gen_server:call(Cache, {remove, Key}, ?DEF_CACHE_TIMEOUT).

%%
%% asynchronous remove entry from cache
-spec(remove_/2 :: (cache(), key()) -> ok).

remove_(Cache, Key) ->
   gen_server:cast(Cache, {remove, Key}).





