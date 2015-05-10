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
     ,?_assertMatch(ok, async(cache:put_(Pid, <<"key-3">>, <<"val-3">>)))
     ,?_assertMatch(ok, async(cache:put_(Pid, <<"key-4">>, <<"val-4">>, 5)))
   ].

get(Pid) ->
   [
      ?_assertMatch(ok, cache:put(Pid, <<"key-1">>, <<"val-1">>))
     ,?_assertMatch(<<"val-1">>, cache:get(Pid, <<"key-1">>))
     ,?_assertMatch(<<"val-1">>, cache:lookup(Pid, <<"key-1">>))
     ,?_assertMatch(true,        cache:has(Pid, <<"key-1">>))

     ,?_assertMatch(ok, async(cache:put_(Pid, <<"key-3">>, <<"val-3">>)))
     ,?_assertMatch(<<"val-3">>, cache:get(Pid, <<"key-3">>))
     ,?_assertMatch(<<"val-3">>, cache:lookup(Pid, <<"key-3">>))
     ,?_assertMatch(true,        cache:has(Pid, <<"key-3">>))

     ,?_assertMatch(undefined,   cache:get(Pid, <<"key-5">>))     
     ,?_assertMatch(undefined,   cache:lookup(Pid, <<"key-5">>))     
     ,?_assertMatch(false,       cache:has(Pid, <<"key-5">>))     
   ].

async(Ref) ->
   receive
      {Ref, X} ->
         X
   end.
