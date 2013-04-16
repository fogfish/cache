-module(cache).

-export([
   start_link/2, drop/1, i/1,
   put/3, put_/3, get/2, has/2, remove/2, remove_/2
]).

%%
%%
start_link(Cache, Opts) ->
   cache_bucket:start_link(Cache, Opts).

%%
%%
drop(Cache) ->
   erlang:exit(whereis(Cache), shutdown).

%%
%%
i(Cache) ->
   gen_server:call(Cache, i).   

%%
%%
put(Cache, Key, Val) ->
   gen_server:call(Cache, {put, Key, Val}).

put_(Cache, Key, Val) ->
   gen_server:cast(Cache, {put, Key, Val}).

%%
%%
get(Cache, Key) ->
   gen_server:call(Cache, {get, Key}).

%%
%%
has(Cache, Key) ->
   gen_server:call(Cache, {has, Key}).

%%
%%
remove(Cache, Key) ->
   gen_server:call(Cache, {remove, Key}).

%%
%%
remove_(Cache, Key) ->
   gen_server:cast(Cache, {remove, Key}).

