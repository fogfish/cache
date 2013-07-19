-module(cache).

-export([start/0]).
-export([
    start_link/0,
    start_link/1,
    start_link/2, 
    i/0, 
    i/1, 
    put/2, 
    put/3, 
    put_ttl/3,
    put_ttl/4,
    get/1, 
    get/2, 
    evict/0, 
    evict/1, 
    stop/0, 
    stop/1
]).

-define(NAME, cache).

%%
start() ->
   AppFile = code:where_is_file(atom_to_list(?MODULE) ++ ".app"),
   {ok, [{application, _, List}]} = file:consult(AppFile),
   Apps = proplists:get_value(applications, List, []),
   lists:foreach(
      fun(X) ->
         ok = case application:start(X) of
            {error, {already_started, X}} -> ok;
            Ret -> Ret
         end
      end,
      Apps
   ),
   application:start(?MODULE).

%%
start_link() ->
    Env = proplists:delete(included_applications, application:get_all_env()),
    start_link(?NAME, Env).
    
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

-spec put(Key::term(), Val::term()) -> ok.

put(Key, Val) ->
    put(?NAME, Key, Val).

-spec put(Cache::atom(), Key::term(), Val::term()) -> ok.

put(Cache, Key, Val) ->
    cache_bucket:put(Cache, Key, Val).

-spec put_ttl(Key::term(), Val::term(), TTL::non_neg_integer()) -> ok.

put_ttl(Key, Val, TTL) when is_integer(TTL) ->
    put_ttl(?NAME, Key, Val, TTL).

-spec put_ttl(Cache::atom(), Key::term(), Val::term(), TTL::non_neg_integer()) -> ok.

put_ttl(Cache, Key, Val, TTL) when is_integer(TTL) ->
    cache_bucket:put_ttl(Cache, Key, Val, TTL * 1000000).

%%
%%
-spec get(Key::term()) -> {ok, value} | none.

get(Key) ->
    get(?NAME, Key).

-spec get(Cache::atom(), Key::term()) -> {ok, value} | none.

get(Cache, Key) ->
    cache_bucket:get(Cache, Key).

%%
%%
-spec evict() -> ok.

evict() ->
    evict(?NAME).

-spec evict(Cache::atom()) -> ok.

evict(Cache) ->
    cache_bucket:evict(Cache).

%%
%%
-spec stop() -> ok.

stop() ->
    stop(?NAME).

-spec stop(Cache::atom()) -> ok.

stop(Cache) ->
    cache_bucket:stop(Cache).

