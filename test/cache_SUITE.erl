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

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

%%
%% primitives
-export([
   lifecycle/1
]).

%%
%% cache basic i/o
-export([
   put/1,
   get/1,
   lookup/1,
   has/1,
   remove/1
]).

%%
%% cache extended i/o
-export([
   acc/1,
   set/1,
   add/1,
   replace/1,
   append/1,
   prepend/1,
   delete/1
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, primitives},
      {group, basic_io},
      {group, extended_io}
   ].

groups() ->
   [
      {primitives, [parallel], 
         [lifecycle]},
      {basic_io, [parallel], 
         [put, get, lookup, has, remove]},
      {extended_io, [parallel], 
         [acc, set, add, replace, append, prepend, delete]}
   ].


%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% cache primitives
%%%
%%%----------------------------------------------------------------------------   

lifecycle(_Config) ->
   {ok, Cache} = cache:start_link([]),
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

get(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:put(Cache, key, val),
   val = cache:get(Cache, key),
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

remove(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true = cache:has(Cache, key),
   ok = cache:remove(Cache, key),   
   false= cache:has(Cache, key),
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

replace(_Config) ->
   {ok, Cache} = cache:start_link([]),
   {error, not_found} = cache:replace(Cache, key, val),
   ok  = cache:set(Cache, key, val),
   ok  = cache:replace(Cache, key, val),
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

prepend(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok  = cache:prepend(Cache, key, a),
   [a] = cache:get(Cache, key),
   ok  = cache:prepend(Cache, key, b),
   [b, a] = cache:get(Cache, key),
   ok  = cache:prepend(Cache, key, c),
   [c, b, a] = cache:get(Cache, key),
   ok  = cache:drop(Cache).

delete(_Config) ->
   {ok, Cache} = cache:start_link([]),
   ok = cache:put(Cache, key, val),
   true = cache:has(Cache, key),
   ok = cache:delete(Cache, key),   
   false= cache:has(Cache, key),
   ok = cache:drop(Cache).


