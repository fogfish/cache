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
