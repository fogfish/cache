-module(cache).

-export([
    start_link/1,
    start_link/2, 
    i/0, 
    i/1, 
    put/2, 
    put/3, 
    get/1, 
    get/2, 
    evict/0, 
    evict/1, 
    stop/0, 
    stop/1
]).

-define(NAME, cache).

%%
%%
start_link(Opts) ->
    start_link(?NAME, Opts).
start_link(Name, Opts) ->
    cache_bucket:start_link(Name, Opts).

i() ->
    i(?NAME).  

i(Cache) ->
    cache_bucket:i(Cache).   

%%
%%
put(Key, Val) ->
    put(?NAME, Key, Val).
put(Cache, Key, Val) ->
    cache_bucket:put(Cache, Key, Val).

%%
%%
get(Key) ->
    get(?NAME, Key).
get(Cache, Key) ->
    cache_bucket:get(Cache, Key).

%%
%%
evict() ->
    evict(?NAME).
evict(Cache) ->
    cache_bucket:evict(Cache).

%%
%%
stop() ->
    stop(?NAME).
stop(Cache) ->
    cache_bucket:stop(Cache).

