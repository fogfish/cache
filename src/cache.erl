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
%%   * procedure to get / lookup multiple keys (e.g. get_, getm, ...)
%%   * unit tests (improve coverage)
%%   * cache read/write through handler
%%   * memcached protocol 
-module(cache).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("cache.hrl").

-export([start/0]).
-export([
   start_link/2,
   drop/1,
   purge/1,
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
   remove_/2,
   acc/3,
   acc_/3,
   % memecached like interface
   set/3,
   set/4,
   set_/3,
   set_/4,
   add/3,
   add/4,
   add_/3,
   add_/4,
   replace/3,
   replace/4,
   replace_/3,
   replace_/4,
   append/3,
   append_/3,
   prepend/3,
   prepend_/3,
   delete/2,
   delete_/2
]).

-export_type([cache/0]).

-type(cache()  :: atom() | {atom(), atom()} | {global, atom()} | pid()).
-type(name()   :: atom() | {global, atom()}).
-type(key()    :: any()).
-type(entity() :: any()).
-type(ttl()    :: integer()).

%%
%% RnD start application
start() ->
   application:start(pq),
   application:start(cache).

%%
%% start new cache bucket, accepted options:
%%    {type,   set | ordered_set} - cache segment type (default set)
%%    {policy, lru | mru} - cache eviction policy
%%    {memory, integer()} - cache memory quota in bytes
%%    {size,   integer()} - cache cardinality quota
%%    {n,      integer()} - number of cache segments
%%    {ttl,    integer()} - default time-to-live in seconds
%%    {quota,  integer()} - frequency of quota check in seconds
%%    {stats,  function() | {Mod, Fun}} - cache statistic aggregate functor 
%%    {heir,   atom() | pid()} - heir of evicted cache segments
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
   gen_server:call(Cache, drop).

%%
%% purge cache
-spec(purge/1 :: (cache()) -> ok).

purge(undefined) ->
   ok;
purge({global, Cache}) ->
   purge(global:whereis_name(Cache));
purge({Cache, Node}) ->
   purge(rpc:call(Node, cache, purge, [Cache]));
purge(Cache)
 when is_atom(Cache) ->
   purge(whereis(Cache));
purge(Cache)
 when is_pid(Cache) ->
   gen_server:call(Cache, purge).


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

%%
%% synchronous in-cache accumulator 
-spec(acc/3  :: (cache(), key(), integer() | [{integer(), integer()}]) -> integer() | undefined).

acc(Cache, Key, Val) ->
   gen_server:call(Cache, {acc, Key, Val}, ?DEF_CACHE_TIMEOUT).

%%
%% asynchronous in-cache accumulator
-spec(acc_/3 :: (cache(), key(), integer() | {integer(), integer()}) -> ok).

acc_(Cache, Key, Val) ->
   gen_server:cast(Cache, {acc, Key, Val}).

%%%----------------------------------------------------------------------------   
%%%
%%% memcached-like interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% synchronous store key/val
-spec(set/3  :: (cache(), key(), entity()) -> ok).
-spec(set/4  :: (cache(), key(), entity(), ttl()) -> ok).

set(Cache, Key, Val) ->
   cache:put(Cache, Key, Val).

set(Cache, Key, Val, TTL) ->
   cache:put(Cache, Key, Val, TTL).

%%
%% asynchronous store key/val
-spec(set_/3 :: (cache(), key(), entity()) -> ok).
-spec(set_/4 :: (cache(), key(), entity(), ttl()) -> ok).

set_(Cache, Key, Val) ->
   cache:put_(Cache, Key, Val).

set_(Cache, Key, Val, TTL) ->
   cache:put_(Cache, Key, Val, TTL).


%%
%% synchronous store key/val only if cache does not already hold data for this key
-spec(add/3  :: (cache(), key(), entity()) -> ok | conflict).
-spec(add/4  :: (cache(), key(), entity(), ttl()) -> ok | conflict).

add(Cache, Key, Val) ->
   gen_server:call(Cache, {add, Key, Val}, ?DEF_CACHE_TIMEOUT).

add(Cache, Key, Val, TTL) ->
   gen_server:call(Cache, {add, Key, Val, TTL}, ?DEF_CACHE_TIMEOUT).

%%
%% asynchronous store key/val only if cache does not already hold data for this key
-spec(add_/3  :: (cache(), key(), entity()) -> ok).
-spec(add_/4  :: (cache(), key(), entity(), ttl()) -> ok).

add_(Cache, Key, Val) ->
   gen_server:cast(Cache, {add, Key, Val}).

add_(Cache, Key, Val, TTL) ->
   gen_server:cast(Cache, {add, Key, Val, TTL}).


%%
%% synchronous store key/val only if cache does hold data for this key
-spec(replace/3  :: (cache(), key(), entity()) -> ok | not_found).
-spec(replace/4  :: (cache(), key(), entity(), ttl()) -> ok | not_found).

replace(Cache, Key, Val) ->
   gen_server:call(Cache, {replace, Key, Val}, ?DEF_CACHE_TIMEOUT).

replace(Cache, Key, Val, TTL) ->
   gen_server:call(Cache, {replace, Key, Val, TTL}, ?DEF_CACHE_TIMEOUT).


%%
%% asynchronous store key/val only if cache does hold data for this key
-spec(replace_/3  :: (cache(), key(), entity()) -> ok).
-spec(replace_/4  :: (cache(), key(), entity(), ttl()) -> ok).

replace_(Cache, Key, Val) ->
   gen_server:cast(Cache, {replace, Key, Val}).

replace_(Cache, Key, Val, TTL) ->
   gen_server:cast(Cache, {replace, Key, Val, TTL}).


%%
%% synchronously add data to existing key after existing data, the operation do not prolong entry ttl
-spec(append/3  :: (cache(), key(), entity()) -> ok | not_found).

append(Cache, Key, Val) ->
   gen_server:call(Cache, {append, Key, Val}, ?DEF_CACHE_TIMEOUT).

%%
%% synchronously add data to existing key after existing data, the operation do not prolong entry ttl
-spec(append_/3  :: (cache(), key(), entity()) -> ok).

append_(Cache, Key, Val) ->
   gen_server:cast(Cache, {append, Key, Val}).


%%
%% synchronously add data to existing key before existing data
-spec(prepend/3  :: (cache(), key(), entity()) -> ok | not_found).

prepend(Cache, Key, Val) ->
   gen_server:call(Cache, {prepend, Key, Val}, ?DEF_CACHE_TIMEOUT).

%%
%% asynchronously add data to existing key before existing data
-spec(prepend_/3  :: (cache(), key(), entity()) -> ok).

prepend_(Cache, Key, Val) ->
   gen_server:cast(Cache, {prepend, Key, Val}).


%%
%% synchronous remove entry from cache
-spec(delete/2  :: (cache(), key()) -> ok).

delete(Cache, Key) ->
   cache:remove(Cache, Key).

%%
%% asynchronous remove entry from cache
-spec(delete_/2 :: (cache(), key()) -> ok).

delete_(Cache, Key) ->
   cache:remove_(Cache, Key).

