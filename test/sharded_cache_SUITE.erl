-module(sharded_cache_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

all() ->
    [
        init_sharded_cache,
        drop_sharded_cache,
        get_shard,
        get_put_delete
    ].


init_per_suite(Config) ->
    ok = application:start(cache),
    Config.


end_per_suite(_Config) ->
    ok.


init_per_testcase(TestCase, Config) ->
    CacheName = list_to_atom("cache_" ++ atom_to_list(TestCase)),
    [{cache_name, CacheName} | Config].


end_per_testcase(_TestCase, Config) ->
    proplists:delete(cache_name, Config).


init_sharded_cache(_Config) ->
    ?assert(is_pid(whereis(cache_sup))),

    ?assertEqual(ok, sharded_cache:init(cache1, 4, cache_sup)),
    ?assertEqual({error, already_exists}, sharded_cache:init(cache1, 4, cache_sup)),

    ?assertEqual(ok, sharded_cache:init(cache2, 8, cache_sup)),
    ?assertEqual({error, already_exists}, sharded_cache:init(cache2, 8, cache_sup)),

    {ok, CacheShards} = application:get_env(cache, cache_shards),
    ?assertEqual(2, maps:size(CacheShards)),
    ?assertMatch(#{cache1 := 4, cache2 := 8}, CacheShards),
    ok.


drop_sharded_cache(_Config) ->
    ?assertEqual(ok, sharded_cache:drop(cache1, cache_sup)),
    ?assertEqual({error, invalid_cache}, sharded_cache:drop(cache1, cache_sup)),
    ?assertEqual(ok, sharded_cache:drop(cache2, cache_sup)),
    ?assertEqual({error, invalid_cache}, sharded_cache:drop(cache2, cache_sup)),
    ?assertEqual({error, invalid_cache}, sharded_cache:drop(some_invalid_cache_name, cache_sup)),
    ok.


get_shard(Config) ->
    CacheName = ?config(cache_name, Config),
    ok = sharded_cache:init(CacheName, 4, cache_sup),
    Shards = [cache_get_shard_1, cache_get_shard_2, cache_get_shard_3, cache_get_shard_4],
    lists:foreach(
        fun(ID) ->
            {ok, Shard} = sharded_cache:get_shard(CacheName, ID),
            ?assert(lists:member(Shard, Shards))
        end,
        lists:seq(1, 100)
    ),
    ?assertEqual({error, invalid_cache}, sharded_cache:get_shard(some_invalid_cache_name, 1)),
    ok = sharded_cache:drop(CacheName, cache_sup),
    ok.


get_put_delete(Config) ->
    CacheName = ?config(cache_name, Config),
    ok = sharded_cache:init(CacheName, 4, cache_sup),

    ?assertEqual({error, not_found}, sharded_cache:get(CacheName, key1)),
    ?assertEqual(ok, sharded_cache:put(CacheName, key1, value1)),
    ?assertEqual({ok, value1}, sharded_cache:get(CacheName, key1)),
    ?assertEqual({error, invalid_cache}, sharded_cache:get(some_invalid_cache_name, key1)),

    {ok, Shard} = sharded_cache:get_shard(CacheName, key1),
    ?assertEqual(value1, cache:get(Shard, key1)),

    ?assertEqual(ok, sharded_cache:delete(CacheName, key1)),
    ?assertEqual({error, not_found}, sharded_cache:get(CacheName, key1)),

    ok = sharded_cache:drop(CacheName, cache_sup),
    ok.