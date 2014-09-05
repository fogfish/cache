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
%% @description
%%   cache unit test
-module(cache_tests).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-include_lib("eunit/include/eunit.hrl").

-define(CACHE, [
   {ttl,   3},   %% time-to-live 3 sec
   {n,     3},   %% 3 cells
   {check, 1}    %% check eviction status 1 sec
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suites
%%%
%%%----------------------------------------------------------------------------   

cache_interface_test_() ->
   {foreach,
      fun init/0
     ,fun free/1
     ,[
         fun i/1
        ,fun heap/1
        ,fun put/1
        ,fun get/1
      ]
   }.


% lru_test_() ->
%    {
%       setup,
%       fun cache_init/0,
%       fun cache_free/1,
%       [
%          {"put",         fun cache_put/0}
%         ,{"has",         fun cache_has/0}
%         ,{"get",         fun cache_get/0}
%         ,{"del",         fun cache_del/0}
%         ,{"acc",         fun cache_acc/0}
%         ,{"lifecycle 1", {timeout, 10000, fun cache_lc1/0}}
%       ]
%    }.

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

init() ->
   {ok, Pid} = cache:start_link(?CACHE),
   Pid.

free(Pid) ->
   erlang:unlink(Pid),
   cache:drop(Pid).


%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

i(Pid) ->
   [
      ?_assertMatch({_, _}, lists:keyfind(heap,   1, cache:i(Pid)))
     ,?_assertMatch({_, _}, lists:keyfind(size,   1, cache:i(Pid)))
     ,?_assertMatch({_, _}, lists:keyfind(memory, 1, cache:i(Pid)))
     ,?_assertMatch({_, _}, lists:keyfind(expire, 1, cache:i(Pid)))
     ,?_assertMatch([_| _], cache:i(Pid, heap))
     ,?_assertMatch([_| _], cache:i(Pid, size))
     ,?_assertMatch([_| _], cache:i(Pid, memory))
     ,?_assertMatch([_| _], cache:i(Pid, expire))
   ].

heap(Pid) ->
   [
      ?_assertMatch(true,   is_integer(cache:heap(Pid, 1)))
     ,?_assertMatch(badarg, cache:heap(Pid, 2))
   ].

put(Pid) ->
   [
      ?_assertMatch(ok, cache:put(Pid, <<"key-1">>, <<"val-1">>))
     ,?_assertMatch(ok, cache:put(Pid, <<"key-2">>, <<"val-2">>, 5))
     ,?_assertMatch(ok, cache:put_(Pid, <<"key-3">>, <<"val-3">>))
     ,?_assertMatch(ok, cache:put_(Pid, <<"key-4">>, <<"val-4">>, 5))
   ].

get(Pid) ->
   [
      ?_assertMatch(ok, cache:put(Pid, <<"key-1">>, <<"val-1">>))
     ,?_assertMatch(<<"val-1">>, cache:get(Pid, <<"key-1">>))
     ,?_assertMatch(<<"val-1">>, cache:lookup(Pid, <<"key-1">>))
     ,?_assertMatch(true,        cache:has(Pid, <<"key-1">>))

     ,?_assertMatch(ok, cache:put_(Pid, <<"key-3">>, <<"val-3">>))
     ,?_assertMatch(<<"val-3">>, cache:get(Pid, <<"key-3">>))
     ,?_assertMatch(<<"val-3">>, cache:lookup(Pid, <<"key-3">>))
     ,?_assertMatch(true,        cache:has(Pid, <<"key-3">>))

     ,?_assertMatch(undefined,   cache:get(Pid, <<"key-5">>))     
     ,?_assertMatch(undefined,   cache:lookup(Pid, <<"key-5">>))     
     ,?_assertMatch(false,       cache:has(Pid, <<"key-5">>))     
   ].


%% @todo - fix ttl and segment expire time





cache_init() ->
   cache:start_link(test, ?CACHE).

cache_free({ok, Pid}) ->
   erlang:unlink(Pid),
   cache:drop(test).

cache_put() ->
   ok = cache:put(test, <<"key">>, <<"val">>).

cache_has() ->
   true  = cache:has(test, <<"key">>),
   false = cache:has(test, <<"yek">>).

cache_get() ->
   <<"val">> = cache:get(test, <<"key">>),
   undefined = cache:get(test, <<"yek">>).

cache_del() ->
   ok = cache:remove(test, <<"key">>),
   ok = cache:remove(test, <<"yek">>).

cache_acc() ->
   undefined = cache:acc(test, <<"acc">>, 10),
   10 = cache:acc(test, <<"acc">>, 10),
   20 = cache:get(test, <<"acc">>),
   badarg = cache:acc(test, <<"acc">>, [{1, 10}]),
   badarg = cache:acc(test, <<"acc1">>,[{1, 10}]),
   ok = cache:put(test, <<"acc1">>, {10,20,30,40}),
   {10, 20, 30, 40} = cache:acc(test, <<"acc1">>,[{1, 10}, {2, 10}]),
   {20, 30, 30, 40} = cache:get(test, <<"acc1">>).

cache_lc1() ->
   ok  = cache:put(test, key, val),
   timer:sleep(1200),
   val = cache:get(test, key),
   timer:sleep(1200),
   val = cache:get(test, key),
   timer:sleep(1200),
   val = cache:get(test, key),
   timer:sleep(3200),
   undefined = cache:get(test, key).


% lifecyle_2_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{ttl,     10},
%    	{evict,  100}
%    ]),
%    ok = cache:put(test, key, val),
%    timer:sleep(6),
%    {ok, val} = cache:get(test, key),
%    timer:sleep(6),
%    {ok, val} = cache:get(test, key),
%    timer:sleep(20),
%    none = cache:get(test, key),
%    cache:stop(test).

% lifecyle_3_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{ttl,   10},
%    	{evict,  5}
%    ]),
%    ok = cache:put(test, key1, val1),
%    timer:sleep(5),
%    ok = cache:put(test, key2, val2),
%    timer:sleep(5),
%    ok = cache:put(test, key3, val3),
%    timer:sleep(5),
%    ok = cache:put(test, key4, val4),

%    none = cache:get(test, key1),
% 	none = cache:get(test, key2),
%    {ok, val3} = cache:get(test, key3),
%    {ok, val4} = cache:get(test, key4),
%    cache:stop(test).

% evict_lru_1_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{policy, lru},
%    	{ttl,    100},
%    	{evict,    5},
%    	{size,    10},
%    	{chunk,    2}
%    ]),
%    lists:foreach(
%    	fun(X) -> cache:put(test, X, X) end,
%    	lists:seq(1, 10)
%    ),
%    timer:sleep(10),
%    {ok, 1} = cache:get(test, 1),
%    cache:put(test, key, val),
%    timer:sleep(10),
%    none = cache:get(test, 2),
%    cache:stop(test).

% evict_lru_2_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{policy, lru},
%    	{ttl,    100},
%    	{evict,  100},
%    	{size,    10},
%    	{chunk,    2}
%    ]),
%    lists:foreach(
%    	fun(X) -> cache:put(test, X, X) end,
%    	lists:seq(1, 10)
%    ),
%    {ok, 1} = cache:get(test, 1),
%    cache:put(test, key, val),
%    cache:evict(test),
%    none = cache:get(test, 2),
%    cache:stop(test).

% evict_mru_1_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{policy, mru},
%    	{ttl,    100},
%    	{evict,    5},
%    	{size,    10},
%    	{chunk,    2}
%    ]),
%    lists:foreach(
%    	fun(X) -> cache:put(test, X, X) end,
%    	lists:seq(1, 10)
%    ),
%    timer:sleep(10),
%    {ok, 1} = cache:get(test, 1),
%    cache:put(test, key, val),
%    timer:sleep(10),
%    none = cache:get(test, key),
%    cache:stop(test).

% evict_mru_2_test() ->
%    cache:start(),
%    {ok, _} = cache:start_link(test, [
%    	{policy, mru},
%    	{ttl,    100},
%    	{evict,  100},
%    	{size,    10},
%    	{chunk,    2}
%    ]),
%    lists:foreach(
%    	fun(X) -> cache:put(test, X, X) end,
%    	lists:seq(1, 10)
%    ),
%    {ok, 1} = cache:get(test, 1),
%    cache:put(test, key, val),
%    cache:evict(test),
%    none = cache:get(test, key),
%    cache:stop(test).
