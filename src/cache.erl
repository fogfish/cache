-module(cache).
-export([start/0]).
-export([start_link/2, put/3, get/2, evict/1, stop/1]).


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
%%
start_link(Name, Opts) ->
   cache_bucket:start_link(Name, Opts).

%%
%%
put(Cache, Key, Val) ->
   cache_bucket:put(Cache, Key, Val).

%%
%%
get(Cache, Key) ->
   cache_bucket:get(Cache, Key).

%%
%%
evict(Cache) ->
   cache_bucket:evict(Cache).

%%
%%
stop(Cache) ->
   cache_bucket:stop(Cache).

