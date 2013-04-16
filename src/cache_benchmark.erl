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
      init()
   catch _:Err ->
      error_logger:error_msg("cache failed: ~p", [Err]),
      halt(1)
   end,
   {ok, undefined}.

%%
%%
run(put, KeyGen, ValGen, S) ->
   Key = KeyGen(),
   case (catch cache:put(cache, Key, ValGen())) of
      ok -> {ok, S};
      E  -> {error, failure(p, Key, E), S}
   end;

run(get, KeyGen, _ValueGen, S) ->
   Key = KeyGen(),
   case (catch cache:get(cache, Key)) of
      Val when is_binary(Val) -> {ok, S};
      undefined               -> {ok, S};
      E -> {error, failure(g, Key, E), S}
   end;

run(remove, KeyGen, _ValueGen, S) ->
   Key = KeyGen(),
   case (catch cache:remove(cache, Key)) of
      ok -> {ok, S};
      E  -> {error, failure(r, Key, E), S}
   end;

run(dump, _KeyGen, _ValueGen, S) ->
   error_logger:info_msg("cache: ~p", [cache:i(cache)]),
   {ok, S}.


%%
%%
init() ->
   case application:start(cache) of
      {error, {already_started, _}} ->
         ok;
      ok ->
         Cache   = basho_bench_config:get(cache, 30000),
         {ok, _} = cache:start_link(cache, Cache)
   end.

%%
%%
failure(Tag, _Key, E) ->
   %io:format("----> ~p~n", [process_info(pns:whereis(kv, Key))]),
   io:format("~s -> ~p~n", [Tag, E]),
   failed.