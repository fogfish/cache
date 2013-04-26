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
%%   @description
%%      basho_bench driver
-module(cache_benchmark).

-export([
   new/1, run/4
]).

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

run(get, KeyGen, _ValueGen, Cache) ->
   Key = KeyGen(),
   case (catch cache:get(Cache, Key)) of
      Val when is_binary(Val)  -> {ok, Cache};
      undefined                -> {ok, Cache};
      E -> {error, failure(g, Key, E), Cache}
   end;

run(remove, KeyGen, _ValueGen, Cache) ->
   Key = KeyGen(),
   case (catch cache:remove(cache, Key)) of
      ok -> {ok, Cache};
      E  -> {error, failure(r, Key, E), Cache}
   end.


%% 
local_init() ->
   case application:start(cache) of
      {error, {already_started, _}} ->
         cache;
      ok ->
         Cache   = basho_bench_config:get(local, undefined),
         {ok, _} = cache:start_link(cache, Cache),
         cache
   end.

%%
%%
failure(Tag, _Key, E) ->
   %io:format("----> ~p~n", [process_info(pns:whereis(kv, Key))]),
   io:format("~s -> ~p~n", [Tag, E]),
   failed.
