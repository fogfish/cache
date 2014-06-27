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
%%   cache basho_bench driver
-module(cache_benchmark).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include_lib("stdlib/include/ms_transform.hrl").

-export([new/1, run/4]).

%%
%%
new(_Id) ->
   try
      lager:set_loglevel(lager_console_backend, basho_bench_config:get(log_level, info)),
      case basho_bench_config:get(cache, undefined) of
         undefined -> {ok, local_init()};
         Cache     -> {ok, Cache}
      end
   catch _:Err ->
      error_logger:error_msg("cache failed: ~p", [Err]),
      halt(1)
   end.

%%
%%
run(put, KeyGen, ValGen, Cache) ->
   Key = KeyGen(),
   case (catch cache:put(Cache, Key, ValGen())) of
      ok -> {ok, Cache};
      E  -> {error, failure(p, Key, E), Cache}
   end;

run(get, KeyGen, _ValGen, Cache) ->
   Key = KeyGen(),
   case (catch cache:get(Cache, Key)) of
      Val when is_binary(Val)  -> {ok, Cache};
      undefined                -> {ok, Cache};
      E -> {error, failure(g, Key, E), Cache}
   end;

run(remove, KeyGen, _ValGen, Cache) ->
   Key = KeyGen(),
   case (catch cache:remove(cache, Key)) of
      ok -> {ok, Cache};
      E  -> {error, failure(r, Key, E), Cache}
   end;

run(select, KeyGen, _ValGen, Cache) ->
   Key0 = KeyGen(),
   Key1 = KeyGen(),
   cache:fold(
      fun(_, Acc) -> Acc + 1 end,
      0,
      ets:fun2ms(fun({Key, Val}) when Key > Key0, Key < Key1 -> Key end),
      cache
   ),
   {ok, Cache}.   


%% 
local_init() ->
   case cache:start() of
      {error, {already_started, _}} ->
         cache;
      ok ->
         Cache   = basho_bench_config:get(local, undefined),
         {ok, _} = cache:start_link(cache, Cache),
         cache
   end.

%%
%%
failure(_Tag, _Key, _E) ->
   %io:format("~s -> ~p~n", [Tag, E]),
   failed.
