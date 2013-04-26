%%
%%   Copyright (c) 2012, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%  This library is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU Lesser General Public License, version 3.0
%%  as published by the Free Software Foundation (the "License").
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%  the License for the specific language governing rights and limitations
%%  under the License.
%% 
%%  You should have received a copy of the GNU Lesser General Public
%%  License along with this library; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%%  USA or retrieve online http://www.opensource.org/licenses/lgpl-3.0.html
%%
%%  @description
%%   segmented in-memory cache, cache memory is split into N segments, 
%%   each segment is associated with quota and ttl, eviction policy applied
%%   at segment level.
-module(cache).
-include("cache.hrl").

-export([
   start_link/2,
   drop/1,
   i/1,
   heap/2,
   put/3, put/4, put_/3, put_/4,
   get/2, lookup/2,
   has/2, ttl/2,
   remove/2, remove_/2
]).

-type(key()    :: any()).
-type(entity() :: any()).

%%
%% start new cache
-spec(start_link/2 :: (atom(), list()) -> {ok, pid()} | {error, any()}).

start_link(Cache, Opts) ->
   cache_bucket:start_link(Cache, Opts).

%%
%% drop cache
-spec(drop/1 :: (atom()) -> ok).

drop(Cache) ->
   erlang:exit(whereis(Cache), shutdown).

%%
%% return cache meta data
-spec(i/1 :: (atom()) -> list()).

i(Cache) ->
   gen_server:call(Cache, i).   

%%
%% return nth cache heap
-spec(heap/2 :: (atom(), integer()) -> integer() | badarg). 

heap(Cache, N) ->
   gen_server:call(Cache, {heap, N}).


%%
%% put entity to cache
-spec(put/3  :: (atom(), key(), entity()) -> ok).
-spec(put_/3 :: (atom(), key(), entity()) -> ok).
-spec(put/4  :: (atom(), key(), entity(), integer()) -> ok).
-spec(put_/4 :: (atom(), key(), entity(), integer()) -> ok).

put(Cache, Key, Val) ->
   gen_server:call(Cache, {put, Key, Val}, ?DEF_IO_TIMEOUT).
put(Cache, Key, Val, TTL) ->
   gen_server:call(Cache, {put, Key, Val, TTL}, ?DEF_IO_TIMEOUT).

put_(Cache, Key, Val) ->
   gen_server:cast(Cache, {put, Key, Val}).
put_(Cache, Key, Val, TTL) ->
   gen_server:cast(Cache, {put, Key, Val, TTL}).

%%
%% get entity from cache, prolong ttl
-spec(get/2 :: (atom(), key()) -> entity() | undefined).

get(Cache, Key) ->
   gen_server:call(Cache, {get, Key}, ?DEF_IO_TIMEOUT).

%%
%% lookup entity at cache do not prolong ttl
-spec(lookup/2 :: (atom(), key()) -> entity() | undefined).

lookup(Cache, Key) ->
   gen_server:call(Cache, {lookup, Key}, ?DEF_IO_TIMEOUT).

%%
%% check entity at cache
-spec(has/2 :: (atom(), key()) -> true | false).

has(Cache, Key) ->
   gen_server:call(Cache, {has, Key}, ?DEF_IO_TIMEOUT).

%%
%% check entity at cache and return estimated ttl
-spec(ttl/2 :: (atom(), key()) -> true | false).

ttl(Cache, Key) ->
   gen_server:call(Cache, {ttl, Key}, ?DEF_IO_TIMEOUT).

%%
%% remove entity from cache
-spec(remove/2  :: (atom(), key()) -> ok).
-spec(remove_/2 :: (atom(), key()) -> ok).

remove(Cache, Key) ->
   gen_server:call(Cache, {remove, Key}, ?DEF_IO_TIMEOUT).

remove_(Cache, Key) ->
   gen_server:cast(Cache, {remove, Key}).







