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
-module(sharded_cache).

-export([
    start_link/2, 
    start_link/3, 
    start/2,
    start/3,
    drop/1,
    get_shard/2,
    get/2,
    put/3,
    delete/2,
    get_stat/1
]).

-export_type([stat/0]).

-define(default_app, cache).
-define(app_env_key, cache_shards).

-type stat() :: #{atom() => integer()}.

%% Public API

-spec start_link(atom(), pos_integer()) -> {ok, pid()} | {error, _}.
start_link(Name, NumShards) ->
    start_link(Name, NumShards, []).

-spec start_link(atom(), pos_integer(), proplists:proplist()) -> {ok, pid()} | {error, _}.
start_link(Name, NumShards, CacheOpts) ->
    cache_shards_sup:start_link(Name, NumShards, CacheOpts).
    
-spec start(atom(), pos_integer()) -> {ok, pid()} | {error, _}.
start(Name, NumShards) ->
    start(Name, NumShards, []).

-spec start(atom(), pos_integer(), proplists:proplist()) -> {ok, pid()} | {error, _}.
start(Name, NumShards, CacheOpts) ->
    case cache_shards_sup:start_link(Name, NumShards, CacheOpts) of
        {ok, Pid} ->
            unlink(Pid),
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

-spec drop(atom()) -> ok | {error, invalid_cache}.
drop(Name) ->
    cache_shards_sup:free(Name).


-spec get_shard(atom(), term()) -> {ok, atom()} | {error, invalid_cache}.
get_shard(Name, Key) ->
    CacheShards = application:get_env(?default_app, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, NumShards} ->
            ID = erlang:phash2(Key) rem NumShards + 1,
            {ok, make_shard_name(Name, ID)};
        error -> 
            {error, invalid_cache}
    end.


-spec get(atom(), term()) -> {ok, term()} | {error, not_found} | {error, invalid_cache}.
get(Name, Key) ->
    case get_shard(Name, Key) of
        {ok, ShardName} ->
            case cache:get(ShardName, Key) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, invalid_cache} = E -> E
    end.


-spec put(atom(), term(), term()) -> ok | {error, invalid_cache}.
put(Name, Key, Value) ->
    case get_shard(Name, Key) of
        {ok, ShardName} -> cache:put(ShardName, Key, Value);
        {error, invalid_cache} = E -> E
    end.


-spec delete(atom(), term()) -> ok | {error, invalid_cache}.
delete(Name, Key) ->
    case get_shard(Name, Key) of
        {ok, ShardName} -> cache:delete(ShardName, Key);
        {error, invalid_cache} = E -> E
    end.


%% Return map #{size => S, memory => N} which aggregates data from all shard and segments.
%% *size* is a number of objects in all ets tables.
%% *memory* is a memory in bytes allocated for all ets tables.

-spec get_stat(atom()) -> {ok, stat()} | {error, invalid_cache}.
get_stat(Name) ->
    CacheShards = application:get_env(?default_app, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, NumShards} ->
            Info = do_get_stat(Name, NumShards),
            {ok, Info};
        error -> {error, invalid_cache}
    end.


%% Inner functions

-spec make_shard_name(atom(), pos_integer()) -> atom().
make_shard_name(Name, ID) ->
    list_to_existing_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(ID)).

-spec do_get_stat(atom(), pos_integer()) -> stat().
do_get_stat(Name, NumShards) ->
    WordSize = erlang:system_info(wordsize),
    lists:foldl(
        fun(ID, #{size := Size, memory := Memory} = Acc) ->
            ShardName = make_shard_name(Name, ID),
            Info = cache:i(ShardName),
            SegmentSizes = proplists:get_value(size, Info),
            SegmentMemory = proplists:get_value(memory, Info),
            Acc#{
                size => Size + lists:sum(SegmentSizes),
                memory => Memory + lists:sum(SegmentMemory) * WordSize
            }
        end,
        #{size => 0, memory => 0},
        lists:seq(1, NumShards)
    ).
