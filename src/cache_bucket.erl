%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
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
%%  @description
%%   cache bucket process
-module(cache_bucket).
-behaviour(gen_server).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("cache.hrl").

-export([
   start_link/2,
   init/1, 
   terminate/2,
   handle_call/3, 
   handle_cast/2, 
   handle_info/2,
   code_change/3
]).

%% internal bucket state
-record(cache, {
   name   = undefined  :: atom(),     %% name of cache bucket
   heap   = []         :: list(),     %% cache heap segments

   n      = ?DEF_CACHE_N      :: integer(),  %% number of segments
   type   = ?DEF_CACHE_TYPE   :: atom(),          
   ttl    = ?DEF_CACHE_TTL    :: integer(),  %% cache time to live
   policy = ?DEF_CACHE_POLICY :: integer(),  %% eviction policy

   cardinality = undefined :: integer(),  %% cache cardinality
   memory      = undefined :: integer(),  %% cache memory limit

   quota  = ?DEF_CACHE_QUOTA  :: integer(),  %% quota enforcement timer
   evict  = undefined                        %% evict timer

  ,stats  = undefined   :: any()       %% stats aggregation functor 
  ,heir   = undefined   :: pid()       %% the heir of evicted cache segment
}).

%% cache segment
-record(heap, {
   id           :: integer(),   %% segment heap
   expire       :: integer(),   %% segment expire time
   cardinality  :: integer(),   %% segment cardinality quota
   memory       :: integer()    %% segment memory quota
}).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

start_link({global, Name}, Opts) ->
   gen_server:start_link({global, Name}, ?MODULE, [Name, Opts], []);

start_link(Name, Opts) ->
   gen_server:start_link({local, Name},  ?MODULE, [Name, Opts], []).

init([Name, Opts]) ->
   {ok, init(Opts, #cache{name=Name})}.

init([{type, X} | T], #cache{}=S) ->
   init(T, S#cache{type=X});

init([{policy, X} | T], #cache{}=S) ->
   init(T, S#cache{policy=X});

init([{memory, X} | Opts], S) ->
   init(Opts, S#cache{memory=X div erlang:system_info(wordsize)});

init([{size,   X} | Opts],  S) ->
   init(Opts, S#cache{cardinality=X});

init([{n,   X} | Opts], S) ->
   init(Opts, S#cache{n = X});

init([{ttl, X} | Opts], S) ->
   init(Opts, S#cache{ttl = X});

init([{quota, X} | Opts], S) ->
   init(Opts, S#cache{quota=X});

init([{stats, X} | Opts], S) ->
   init(Opts, S#cache{stats=X});

init([{heir,  X} | Opts], S) ->
   init(Opts, S#cache{heir=X});

init([_ | Opts], S) ->
   init(Opts, S);

init([], S) ->
   random:seed(os:timestamp()),
   Evict = cache_util:mdiv(S#cache.ttl, S#cache.n),
   init_heap(
      S#cache{
         evict  = cache_util:timeout(Evict * 1000,         evict),
         quota  = cache_util:timeout(S#cache.quota * 1000, quota)
      }
   ).

%%
%%     
terminate(_Reason, S) ->
   lists:foreach(
      fun(X) ->
         destroy_heap(X#heap.id, S#cache.heir)
      end,
      S#cache.heap
   ).

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

handle_call({put, Key, Val}, _, S) ->
   {reply, ok, cache_put(Key, Val, S)};

handle_call({put, Key, Val, TTL}, _, S) ->
   {reply, ok, cache_put(Key, Val, cache_util:now() + TTL, S)};

handle_call({get, Key}, _, S) ->
   {reply, cache_get(Key, S), S};

handle_call({lookup, Key}, _, S) ->
   {reply, cache_lookup(Key, S), S};

handle_call({has, Key}, _, S) ->
   {reply, cache_has(Key, S), S};

handle_call({ttl, Key}, _, S) ->
   {reply, cache_ttl(Key, S), S};

handle_call({remove, Key}, _, S) ->
   {reply, ok, cache_remove(Key, S)};

handle_call({acc, Key, Val}, _, S) ->
   {Reply, NS} = cache_acc(Key, Val, S),
   {reply, Reply, NS};

handle_call({add, Key, Val}, _, S) ->
   case cache_has(Key, S) of
      true  ->
         {reply, conflict, S};
      false ->
         {reply, ok, cache_put(Key, Val, S)}
   end;

handle_call({add, Key, Val, TTL}, _, S) ->
   case cache_has(Key, S) of
      true  ->
         {reply, conflict, S};
      false ->
         {reply, ok, cache_put(Key, Val, cache_util:now() + TTL, S)}
   end;

handle_call({replace, Key, Val}, _, S) ->
   case cache_has(Key, S) of
      true  ->
         {reply, ok, cache_put(Key, Val, S)};
      false ->
         {reply, not_found, S}
   end;

handle_call({replace, Key, Val, TTL}, _, S) ->
   case cache_has(Key, S) of
      true  ->
         {reply, ok, cache_put(Key, Val, cache_util:now() + TTL, S)};
      false ->
         {reply, not_found, S}
   end;

handle_call({prepend, Key, Val}, _, S) ->
   % @todo: reduce one write
   case cache_get(Key, S) of
      undefined  ->
         {reply, ok, cache_put(Key, [Val], S)};
      X when is_list(X) ->
         {reply, ok, cache_put(Key, [Val|X], S)};
      X ->
         {reply, ok, cache_put(Key, [Val,X], S)}
   end;

handle_call({append, Key, Val}, _, S) ->
   % @todo: reduce one write
   case cache_get(Key, S) of
      undefined  ->
         {reply, ok, cache_put(Key, [Val], S)};
      X when is_list(X) ->
         {reply, ok, cache_put(Key, X++[Val], S)};
      X ->
         {reply, ok, cache_put(Key, [X, Val], S)}
   end;

handle_call(i, _, S) ->
   Heap   = [X#heap.id     || X <- S#cache.heap],
   Expire = [X#heap.expire || X <- S#cache.heap],
   Size   = [ets:info(X#heap.id, size)   || X <- S#cache.heap],
   Memory = [ets:info(X#heap.id, memory) || X <- S#cache.heap],
   {reply, [{heap, Heap}, {expire, Expire}, {size, Size}, {memory, Memory}], S};

handle_call({heap, N}, _, S) ->
   try
      H = lists:nth(N, S#cache.heap),
      {reply, H#heap.id, S}
   catch _:_ ->
      {reply, badarg,    S}
   end;

handle_call(drop, _, S) ->
   {stop, normal, ok, S};

handle_call(purge, _, S) ->
   lists:foreach(
      fun(X) ->
         destroy_heap(X#heap.id, S#cache.heir)
      end,
      S#cache.heap
   ),
   {reply, ok, init_heap(S#cache{heap = []})};

handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast({put, Key, Val}, S) ->
   {noreply, cache_put(Key, Val, S)};

handle_cast({put, Key, Val, TTL}, S) ->
   {noreply, cache_put(Key, Val, cache_util:now() + TTL, S)};

handle_cast({remove, Key}, S) ->
   {noreply, cache_remove(Key, S)};

handle_cast({acc, Key, Val}, S) ->
   {_, NS} = cache_acc(Key, Val, S),
   {noreply, NS};

handle_cast({add, Key, Val}, S) ->
   case cache_has(Key, S) of
      true  ->
         {noreply, S};
      false ->
         {noreply, cache_put(Key, Val, S)}
   end;

handle_cast({add, Key, Val, TTL}, S) ->
   case cache_has(Key, S) of
      true  ->
         {noreply, S};
      false ->
         {noreply, cache_put(Key, Val, cache_util:now() + TTL, S)}
   end;

handle_cast({replace, Key, Val}, S) ->
   case cache_has(Key, S) of
      true  ->
         {noreply, cache_put(Key, Val, S)};
      false ->
         {noreply, S}
   end;

handle_cast({replace, Key, Val, TTL}, S) ->
   case cache_has(Key, S) of
      true  ->
         {noreply, cache_put(Key, Val, cache_util:now() + TTL, S)};
      false ->
         {noreply, S}
   end;

handle_cast({prepend, Key, Val}, S) ->
   % @todo: reduce one write
   case cache_get(Key, S) of
      undefined  ->
         {noreply, cache_put(Key, [Val], S)};
      X when is_list(X) ->
         {noreply, cache_put(Key, [Val|X], S)};
      X ->
         {noreply, cache_put(Key, [Val,X], S)}
   end;

handle_cast({append, Key, Val}, S) ->
   % @todo: reduce one write
   case cache_get(Key, S) of
      undefined  ->
         {noreply, cache_put(Key, [Val], S)};
      X when is_list(X) ->
         {noreply, cache_put(Key, X++[Val], S)};
      X ->
         {noreply, cache_put(Key, [X, Val], S)}
   end;

handle_cast(_, S) ->
   {noreply, S}.

%%
%%
handle_info(evict, S) ->
   Now = cache_util:now(),
   case lists:last(S#cache.heap) of
      H when H#heap.expire =< Now ->
         {noreply,
            free_heap(
               S#cache{evict = cache_util:timeout(S#cache.evict, evict)}
            )
         };
      _ ->
         {noreply,
            init_heap(
               S#cache{evict = cache_util:timeout(S#cache.evict, evict)} 
            )
         }
   end;

handle_info(quota, S) ->
   case is_heap_out_of_quota(hd(S#cache.heap)) of
      true  ->
         case length(S#cache.heap) of
            N when N =:= S#cache.n ->
               {noreply,
                  free_heap(
                     S#cache{quota = cache_util:timeout(S#cache.quota, quota)}
                  )
               };
            _ ->
               {noreply,
                  init_heap(
                     S#cache{quota = cache_util:timeout(S#cache.quota, quota)}
                  )
               }
         end;
      false ->
         {noreply,
            S#cache{
               quota = cache_util:timeout(S#cache.quota, quota)
            }
         }
   end;

handle_info(_, S) ->
   {noreply, S}.

%%
%% 
code_change(_Vsn, S, _Extra) ->
   {ok, S}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% insert value to cache
cache_put(Key, Val, #cache{}=S) ->
   [Head | Tail] = S#cache.heap,
   true = ets:insert(Head#heap.id, {Key, Val}),
   lists:foreach(
      fun(X) -> ets:delete(X#heap.id, Key) end,
      Tail
   ),
   cache_util:stats(S#cache.stats, {cache, S#cache.name, put}),
   ?DEBUG("cache ~p: put ~p to heap ~p~n", [S#cache.name, Key, Head#heap.id]),
   S.

cache_put(Key, Val, Expire, #cache{}=S) ->
   case lists:splitwith(fun(X) -> X#heap.expire > Expire end, S#cache.heap) of
      {[],  _Tail} ->
         cache_put(Key, Val, S);
      {Head, Tail} ->
         [Heap | Rest] = lists:reverse(Head),
         true = ets:insert(Heap#heap.id, {Key, Val}),
         lists:foreach(
            fun(X) -> ets:delete(X#heap.id, Key) end,
            Rest ++ Tail
         ),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, put}),
         ?DEBUG("cache ~p: put ~p to heap ~p~n", [S#cache.name, Key, Heap#heap.id]),
         S
   end.

%%
%% get cache value
cache_get(Key, #cache{policy=mru}=S) ->
   % cache MRU should not move key anywhere because
   % cache always evicts last generation
   % fall-back to cache lookup
   cache_lookup(Key, S);

cache_get(Key, #cache{}=S) ->
   Head = hd(S#cache.heap),
   case heap_lookup(Key, S#cache.heap) of
      undefined   ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {Heap, Val} when Heap#heap.id =:= Head#heap.id ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val;
      {Heap, Val} ->
         true = ets:insert(Head#heap.id, {Key, Val}),
         _    = ets:delete(Heap#heap.id, Key),
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end.

%%
%% lookup cache value
cache_lookup(Key, #cache{}=S) ->
   case heap_lookup(Key, S#cache.heap) of
      undefined   ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {_Heap, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end.

%%
%% check if key exists
cache_has(Key, #cache{}=S) ->
   case heap_has(Key, S#cache.heap) of
      false   ->
         false;
      _Heap   ->
         ?DEBUG("cache ~p: has ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         true
   end.

%%
%% check key ttl
cache_ttl(Key, #cache{}=S) ->
   case heap_has(Key, S#cache.heap) of
      false   ->
         undefined;
      Heap    ->
         Heap#heap.expire - cache_util:now()
   end.

%%
%%
cache_remove(Key, #cache{}=S) ->
   lists:foreach(
      fun(X) -> ets:delete(X#heap.id, Key) end,
      S#cache.heap
   ),
   cache_util:stats(S#cache.stats, {cache, S#cache.name, remove}),
   ?DEBUG("cache ~p: remove ~p~n", [S#cache.name, Key]),
   S.

%%
%% @todo: reduce one write
cache_acc(Key, Val, S)
 when is_integer(Val) ->
   case cache_get(Key, S) of
      undefined ->
         {undefined, cache_put(Key, Val, S)};
      X when is_integer(X) ->
         {X, cache_put(Key, X + Val, S)};
      X when is_tuple(X) ->
         {erlang:element(1, X), cache_put(Key, tuple_acc({1, Val}, X), S)};
      _  ->
         {badarg, S}
   end;
cache_acc(Key, Val, S) ->
   case cache_get(Key, S) of
      X when is_tuple(X) ->
         {X, cache_put(Key, tuple_acc(Val, X), S)};
      _  ->
         {badarg, S}
   end.

tuple_acc({Pos, Val}, X) ->
   erlang:setelement(Pos, X, erlang:element(Pos, X) + Val);
tuple_acc(List, X) ->
   lists:foldl(
      fun({Pos, Val}, Acc) ->
         erlang:setelement(Pos, Acc, erlang:element(Pos, Acc) + Val)
      end,
      X,
      List 
   ).

%%
%%
heap_lookup(Key, [H | Tail]) ->
   case ets:lookup(H#heap.id, Key) of
      []         -> heap_lookup(Key, Tail);
      [{_, Val}] -> {H, Val}
   end;

heap_lookup(_Key, []) ->
   undefined.

%%
%%
heap_has(Key, [H | Tail]) ->
   case ets:member(H#heap.id, Key) of
      false  -> heap_has(Key, Tail);
      true   -> H
   end;

heap_has(_Key, []) ->
   false.


%%
%% init cache heap
init_heap(#cache{}=S) ->
   Id = ets:new(undefined, [S#cache.type, protected]),
   ?DEBUG("cache ~p: init heap ~p~n", [S#cache.name, Id]),
   Heap = #heap{
      id          = Id,
      expire      = cache_util:madd(S#cache.ttl,  cache_util:now()),
      cardinality = cache_util:mdiv(S#cache.cardinality, S#cache.n),
      memory      = cache_util:mdiv(S#cache.memory,      S#cache.n)
   },
   S#cache{
      heap = [Heap | S#cache.heap]
   }.

%%
%%
free_heap(#cache{}=S) ->
   [H | Tail] = lists:reverse(S#cache.heap),
   Size = ets:info(H#heap.id, size),
   cache_util:stats(S#cache.stats, {cache, S#cache.name, evicted}, Size),
   destroy_heap(H#heap.id, S#cache.heir),
   ?DEBUG("cache ~p: free heap ~p~n", [S#cache.name, H#heap.id]),
   init_heap(
      S#cache{
         heap = lists:reverse(Tail)
      }
   ).
   
destroy_heap(Id, undefined) ->
   ets:delete(Id);
destroy_heap(Id, Heir)
 when is_pid(Heir) ->
   ets:give_away(Id, Heir, evicted);
destroy_heap(Id, Heir)
 when is_atom(Heir) ->
   case erlang:whereis(Heir) of
      undefined ->
         ets:delete(Id);
      Pid       ->
         ets:give_away(Id, Pid, evicted)
   end.


%%
%% heap policy check
is_heap_out_of_quota(#heap{}=H) ->
   is_out_of_memory(H) orelse is_out_of_capacity(H).

is_out_of_capacity(#heap{cardinality=undefined}) ->
   false;
is_out_of_capacity(#heap{cardinality=N}=H) ->
   ets:info(H#heap.id, size) >= N.

is_out_of_memory(#heap{memory=undefined}) ->
   false;
is_out_of_memory(#heap{memory=N}=H) ->
   ets:info(H#heap.id, memory) >= N.


