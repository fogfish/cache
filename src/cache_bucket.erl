%%
%%   Copyright (c) 2012, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%  This library is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU Lesser General Public License, version 3.0
%%  as published by the Free Software Foundation (the "License").
%%
%%  Software distributed under the License is distributed on an "AS IS"
%%  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%  the License for the specific language governing rights and limitations
%%  under the License.
%% 
%%  You should have received a copy of the GNU Lesser General Public
%%  License along with this library; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%%  USA or retrieve online http://www.opensource.org/licenses/lgpl-3.0.html
%%
%%  @description
%%
-module(cache_bucket).
-behaviour(gen_server).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-include("cache.hrl").

-export([
   start_link/2,
   init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
]).

%%
%%
-record(cache, {
   name          :: atom(),     %% name of cache bucket
   heap   = []   :: list(),     %% cache heap segments

   n      = ?DEF_CACHE_N      :: integer(),  %% number of segments          
   ttl    = ?DEF_CACHE_TTL    :: integer(),  %% chunk time to live
   policy = ?DEF_CACHE_POLICY :: integer(),  %% eviction policy

   cardinality  :: integer(),  %% cache cardinality
   memory       :: integer(),  %% cache memory limit

   quota  = ?DEF_CACHE_QUOTA  :: integer(),  %% quota enforcement timer
   evict  = undefined,                       %% evict timer

   stats        :: any()       %% stats functor 
}).

-record(heap, {
   id           :: integer(),   %% segment heap
   expire       :: integer(),   %% segment expire time
   cardinality  :: integer(),   %% segment cardinality quota
   memory       :: integer()    %% segment memory quota
}).

%%
%%
start_link({global, Name}, Opts) ->
   gen_server:start_link({global, Name}, ?MODULE, [Name, Opts], []);

start_link(Name, Opts) ->
   gen_server:start_link({local, Name},  ?MODULE, [Name, Opts], []).

init([Name, Opts]) ->
   {ok, init(Opts, #cache{name=Name})}.

init([{policy, X} | T], #cache{}=S) ->
   init(T, S#cache{policy=X});

init([{memory, X} | Opts], S) ->
   init(Opts, S#cache{memory=X div erlang:sysinfo(wordsize)});

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

init([_ | Opts], S) ->
   init(Opts, S);

init([], S) ->
   random:seed(erlang:now()),
   Evict = cache_util:mdiv(S#cache.ttl, S#cache.n),
   init_heap(
      S#cache{
         evict = cache_util:timeout(Evict * 1000,         evict),
         quota = cache_util:timeout(S#cache.quota * 1000, quota)
      }
   ).

%%
%%     
terminate(_Reason, _S) ->
   %cache_heap:free(S#cache.heap),
   ok.

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

handle_cast(_, S) ->
   {noreply, S}.

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
                     S#cache{quota = cache_util:timeout(S#cache.evict, quota)}
                  )
               };
            _ ->
               {noreply,
                  init_heap(
                     S#cache{quota = cache_util:timeout(S#cache.evict, quota)}
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

cache_put(Key, Val, Time, #cache{}=S) ->
   case lists:splitwith(fun(X) -> X#heap.expire > Time end, S#cache.heap) of
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

cache_get(Key, #cache{policy=mru}=S) ->
   % cache always evicts last generation
   % MRU caches do not prolong recently used entity
   case heap_lookup(Key, S#cache.heap) of
      undefined   ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {Heap, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end;

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

cache_lookup(Key, #cache{}=S) ->
   case heap_lookup(Key, S#cache.heap) of
      undefined   ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {Heap, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end.

cache_has(Key, #cache{}=S) ->
   case heap_has(Key, S#cache.heap) of
      false   ->
         false;
      Heap    ->
         ?DEBUG("cache ~p: has ~p at cell ~p~n", [S#cache.name, Key, Heap#heap.id]),
         true
   end.

cache_ttl(Key, #cache{}=S) ->
   case heap_has(Key, S#cache.heap) of
      false   ->
         undefined;
      Heap    ->
         Heap#heap.expire - cache_util:now()
   end.

cache_remove(Key, #cache{}=S) ->
   lists:foreach(
      fun(X) -> ets:delete(X#heap.id, Key) end,
      S#cache.heap
   ),
   cache_util:stats(S#cache.stats, {cache, S#cache.name, remove}),
   ?DEBUG("cache ~p: remove ~p~n", [S#cache.name, Key]),
   S.

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


% %%
% %%
% remove(Key, S) ->
%    Cell = evict_key(Key, cache_heap:cells(S#cache.heap)),
%    ?DEBUG("cache ~p: remove ~p in cell ~p~n", [S#cache.name, Key, Cell]),
%    S.

% %%
% %%
% member(Key, S) ->
%    dowhile(
%       fun(X) -> ets:member(X, Key) end,
%       cache_heap:cells(S#cache.heap)
%    ).  


%%
%% init cache heap
init_heap(#cache{}=S) ->
   Id = ets:new(undefined, [set, protected]),
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
   ets:delete(H#heap.id),
   cache_util:stats(S#cache.stats, {cache, S#cache.name, evicted}, Size),
   ?DEBUG("cache ~p: free heap ~p~n", [S#cache.name, H#heap.id]),
   init_heap(
      S#cache{
         heap = lists:reverse(Tail)
      }
   ).
   
%%
%%
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


