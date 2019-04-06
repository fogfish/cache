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
-module(cache_shards_sup).
-behaviour(supervisor).

-export([start_link/3, init/1, free/1]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%
%%
start_link(Name, NumShards, Opts) ->
   case supervisor:start_link({local, Name}, ?MODULE, [Name, NumShards, Opts]) of
      {ok, Sup} ->
         ok = register_cache_shards(Name, NumShards),
         {ok, Sup};
      {error, _} = Error ->
         Error
   end.

free(Name) ->
   case erlang:whereis(Name) of
      undefined ->
         {error, invalid_cache};
      Pid ->
         ok = deregister_cache_shards(Name),
         Ref = monitor(process, Pid),
         exit(Pid, shutdown),
         receive
            {'DOWN', Ref, process, Pid, _Reason} ->
               ok
            after 30000 ->
               error(exit_timeout)
         end
   end.

init([Name, NumShards, Opts]) ->
   {ok,
      {
         {one_for_one, 4, 1800},
         [shard(make_shard_name(Name, Id), Opts) || Id <- lists:seq(1, NumShards)]
      }
   }.

shard(ShardName, CacheOpts) ->
   #{
      id => ShardName,
      start => {cache, start_link, [ShardName, CacheOpts]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [cache]
   }.

make_shard_name(Name, ID) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(ID)).

register_cache_shards(Name, NumShards) ->
   CacheShards1 = application:get_env(cache, cache_shards, #{}),
   CacheShards2 = CacheShards1#{Name => NumShards},
   application:set_env(cache, cache_shards, CacheShards2).

deregister_cache_shards(Name) ->
   CacheShards1 = application:get_env(cache, cache_shards, #{}),
   CacheShards2 = maps:remove(Name, CacheShards1),
   application:set_env(cache, cache_shards, CacheShards2).
