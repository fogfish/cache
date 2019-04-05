-module(sharded_cache).

-export([
    init/3, init/4, init/5,
    drop/2, drop/3,
    get_shard/2, get_shard/3,
    get/2, get/3,
    put/3, put/4,
    delete/2, delete/3,
    get_stat/1, get_stat/2
]).

-export_type([stat/0]).

-define(default_app, cache).
-define(app_env_key, cache_shards).

-type stat() :: #{atom() => integer()}.

%% Public API

-spec init(atom(), pos_integer(), supervisor:sup_ref()) -> ok | {error, already_exists}.
init(Name, NumShards, Sup) ->
    init(Name, NumShards, [], ?default_app, Sup).


-spec init(atom(), pos_integer(), proplists:proplist(), supervisor:sup_ref()) ->
    ok | {error, already_exists}.
init(Name, NumShards, CacheOpts, Sup) ->
    init(Name, NumShards, CacheOpts, ?default_app, Sup).


-spec init(atom(), pos_integer(), proplists:proplist(), atom(), supervisor:sup_ref()) ->
    ok | {error, already_exists}.
init(Name, NumShards, CacheOpts, Application, Sup) ->
    CacheShards = application:get_env(Application, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, _} -> {error, already_exists};
        error ->
            CacheShards2 = CacheShards#{Name => NumShards},
            application:set_env(Application, ?app_env_key, CacheShards2),
            start_shards(Name, NumShards, CacheOpts, Sup)
    end.


-spec drop(atom(), supervisor:sup_ref()) -> ok | {error, term()}.
drop(Name, Sup) ->
    drop(Name, ?default_app, Sup).


-spec drop(atom(), atom(), supervisor:sup_ref()) -> ok | {error, invalid_cache}.
drop(Name, Application, Sup) ->
    CacheShards = application:get_env(Application, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, NumShards} ->
            %% tricky situation: we can't be sure we have proper Sup here
            CacheShards2 = maps:remove(Name, CacheShards),
            application:set_env(Application, ?app_env_key, CacheShards2),
            stop_shards(Name, NumShards, Sup);
        error -> {error, invalid_cache}
    end.


-spec get_shard(atom(), term()) -> {ok, atom()} | {error, invalid_cache}.
get_shard(Name, Key) ->
    get_shard(Name, Key, ?default_app).


-spec get_shard(atom(), term(), atom()) -> {ok, atom()} | {error, invalid_cache}.
get_shard(Name, Key, Application) ->
    CacheShards = application:get_env(Application, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, NumShards} ->
            ID = erlang:phash2(Key) rem NumShards + 1,
            {ok, make_shard_name(Name, ID)};
        error -> {error, invalid_cache}
    end.


-spec get(atom(), term()) -> {ok, term()} | {error, not_found} | {error, invalid_cache}.
get(Name, Key) ->
    get(Name, Key, ?default_app).


-spec get(atom(), term(), atom()) -> {ok, term()} | {error, not_found} | {error, invalid_cache}.
get(Name, Key, Application) ->
    case get_shard(Name, Key, Application) of
        {ok, ShardName} ->
            case cache:get(ShardName, Key) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, invalid_cache} = E -> E
    end.


-spec put(atom(), term(), term()) -> ok | {error, invalid_cache}.
put(Name, Key, Value) ->
    put(Name, Key, Value, ?default_app).


-spec put(atom(), term(), term(), atom()) -> ok | {error, invalid_cache}.
put(Name, Key, Value, Application) ->
    case get_shard(Name, Key, Application) of
        {ok, ShardName} -> cache:put(ShardName, Key, Value);
        {error, invalid_cache} = E -> E
    end.


-spec delete(atom(), term()) -> ok | {error, invalid_cache}.
delete(Name, Key) ->
    delete(Name, Key, ?default_app).


-spec delete(atom(), term(), atom()) -> ok | {error, invalid_cache}.
delete(Name, Key, Application) ->
    case get_shard(Name, Key, Application) of
        {ok, ShardName} -> cache:delete(ShardName, Key);
        {error, invalid_cache} = E -> E
    end.


%% Return map #{size => S, memory => N} which aggregates data from all shard and segments.
%% *size* is a number of objects in all ets tables.
%% *memory* is a memory in bytes allocated for all ets tables.

-spec get_stat(atom()) -> {ok, stat()} | {error, invalid_cache}.
get_stat(Name) ->
    get_stat(Name, ?default_app).


-spec get_stat(atom(), atom()) -> {ok, stat()} | {error, invalid_cache}.
get_stat(Name, Application) ->
    CacheShards = application:get_env(Application, ?app_env_key, #{}),
    case maps:find(Name, CacheShards) of
        {ok, NumShards} ->
            Info = do_get_stat(Name, NumShards),
            {ok, Info};
        error -> {error, invalid_cache}
    end.



%% Inner functions

-spec make_shard_name(atom(), pos_integer()) -> atom().
make_shard_name(Name, ID) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(ID)).


-spec start_shards(atom(), pos_integer(), proplists:proplist(), supervisor:sup_ref()) -> ok.
start_shards(Name, NumShards, CacheOpts, Sup) ->
    lists:foreach(
        fun(ID) -> start_shard(make_shard_name(Name, ID), CacheOpts, Sup) end,
        lists:seq(1, NumShards)
    ),
    ok.


-spec start_shard(atom(), proplists:proplist(), supervisor:sup_ref()) -> ok.
start_shard(ShardName, CacheOpts, Sup) ->
    ChildSpec = #{
        id => ShardName,
        start => {cache, start_link, [ShardName, CacheOpts]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [cache]
    },
    {ok, _} = supervisor:start_child(Sup, ChildSpec),
    ok.


-spec stop_shards(atom(), pos_integer(), supervisor:sup_ref()) -> ok | no_return().
stop_shards(Name, NumShards, Sup) ->
    lists:foreach(
        fun(ID) ->
            case stop_shard(make_shard_name(Name, ID), Sup) of
                ok -> ok;
                {error, Error} -> throw({invalid_sup, Sup, Error})
            end
        end,
        lists:seq(1, NumShards)
    ).


-spec stop_shard(atom(), supervisor:sup_ref()) -> ok | {error, term()}.
stop_shard(ShardName, Sup) ->
    supervisor:terminate_child(Sup, ShardName).


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
