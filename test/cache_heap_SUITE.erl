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
-module(cache_heap_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-compile(export_all).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

init_per_testcase(_, Config) ->
   meck:new(cache_util, [passthrough]),
   meck:expect(cache_util, now, fun() -> 0 end),
   Config.

end_per_testcase(_, _) ->
   meck:unload(cache_util).

%%
heap_init(_) ->
   {heap, set, 10, 6, 100, 1280, Segments} = cache_heap:new(set, 10, 60, 1000, 102400),
   Expect = lists:seq(6, 60, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].


%%
heap_purge(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {heap, set, 10, 6, 100, 1280, Segments} = cache_heap:purge(undefined, Heap),
   Expect = lists:seq(6, 60, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_slip_ok(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {ok, Heap} = cache_heap:slip(undefined, Heap).

%%
heap_slip_ttl(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   meck:expect(cache_util, now, fun() -> 6 end),
   {ttl,
      {heap, set, 10, 6, 100, 1280, Segments}
   } = cache_heap:slip(undefined, Heap),
   Expect = lists:seq(12, 66, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_split_last(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {{60, _}, Segments} = cache_heap:split(Heap),
   Expect = lists:seq(6, 54, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_split_45(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {{48, _}, Segments} = cache_heap:split(45, Heap),
   Expect = lists:seq(6, 42, 6) ++ lists:seq(54, 60, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_split_15(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {{18, _}, Segments} = cache_heap:split(15, Heap),
   Expect = lists:seq(6, 12, 6) ++ lists:seq(24, 60, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_split_65(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {{60, _}, Segments} = cache_heap:split(65, Heap),
   Expect = lists:seq(6, 54, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].

%%
heap_split_0(_) ->
   Heap = cache_heap:new(set, 10, 60, 1000, 102400),
   {{6, _}, Segments} = cache_heap:split(0, Heap),
   Expect = lists:seq(12, 60, 6),
   Expect = [Expire || {Expire, _} <- queue:to_list(Segments)].


