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
   start_link/1,
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
   name   = undefined         :: atom()     %% name of cache bucket
  ,heap   = undefined         :: list()     %% cache heap segments
  ,n      = ?DEF_CACHE_N      :: integer()  %% number of segments
  ,policy = ?DEF_CACHE_POLICY :: integer()  %% eviction policy
  ,check  = ?DEF_CACHE_CHECK  :: integer()  %% status check timeout
  ,evict  = undefined         :: integer()  %% evict timeout
  ,stats  = undefined         :: any()      %% stats aggregation functor 
  ,heir   = undefined         :: pid()      %% the heir of evicted cache segment
}).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Opts) ->
   gen_server:start_link(?MODULE, [undefined, Opts], []).

start_link({global, Name}, Opts) ->
   gen_server:start_link({global, Name}, ?MODULE, [Name, Opts], []);

start_link(Name, Opts) ->
   gen_server:start_link({local, Name},  ?MODULE, [Name, Opts], []).

init([Name, Opts]) ->
   {ok, init(Opts, Opts, #cache{name=Name})}.

init([{policy, X} | Tail], Opts, State) ->
   init(Tail, Opts, State#cache{policy=X});
init([{n,      X} | Tail], Opts, State) ->
   init(Tail, Opts, State#cache{n=X});
init([{check,  X} | Tail], Opts, State) ->
   init(Tail, Opts, State#cache{check=X * 1000});
init([{stats,  X} | Tail], Opts, State) ->
   init(Tail, Opts, State#cache{stats=X});
init([{heir,   X} | Tail], Opts, State) ->
   init(Tail, Opts, State#cache{heir=X});
init([_ | Tail], Opts, State) ->
   init(Tail, Opts, State);
init([], Opts, State) ->
   Type = proplists:get_value(type,   Opts, ?DEF_CACHE_TYPE),
   TTL  = proplists:get_value(ttl,    Opts, ?DEF_CACHE_TTL),
   Size = proplists:get_value(size,   Opts),
   Mem  = proplists:get_value(memory, Opts),
   Evict= cache_util:mmul(cache_util:mdiv(TTL,  State#cache.n), 1000),
   Heap = cache_heap:new(
      Type
     ,cache_util:mdiv(TTL,  State#cache.n)
     ,cache_util:mdiv(Size, State#cache.n)
     ,cache_util:mdiv(cache_util:mdiv(Mem,  State#cache.n), erlang:system_info(wordsize))
   ),
   (catch erlang:send_after(State#cache.check, self(), check_heap)),
   (catch erlang:send_after(Evict, self(), evict_heap)),
   State#cache{heap=Heap, evict=Evict}.

%%
%%     
terminate(_Reason, State) ->
   cache_heap:purge(State#cache.heap, State#cache.heir),
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

handle_call(i, _, State) ->
   Heap   = cache_heap:refs(State#cache.heap),
   Refs   = [X || {_, X} <- Heap],
   Expire = [X || {X, _} <- Heap],
   Size   = [ets:info(X, size)   || {_, X} <- Heap],
   Memory = [ets:info(X, memory) || {_, X} <- Heap],
   {reply, [{heap, Refs}, {expire, Expire}, {size, Size}, {memory, Memory}], State};

handle_call({heap, N}, _, State) ->
   try
      {_, Ref} = lists:nth(N, cache_heap:refs(State#cache.heap)),
      {reply, Ref, State}
   catch _:_ ->
      {reply, badarg, State}
   end;

handle_call(drop, _, State) ->
   {stop, normal, ok, State};

handle_call(purge, _, State) ->
   {reply, ok, State#cache{heap=cache_heap:purge(State#cache.heap, State#cache.heir)}};

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
handle_info(check_heap, #cache{n=N, check=Check}=State) ->
   erlang:send_after(Check, self(), check_heap),
   Heap = cache_heap:slip(State#cache.heap),
   case cache_heap:size(Heap) of
      X when X > N ->
         {noreply, State#cache{heap=cache_heap:drop(Heap, State#cache.heir)}};
      _ ->
         {noreply, State#cache{heap=Heap}}
   end;

handle_info(evict_heap, #cache{n=N, evict=Evict}=State) ->
   erlang:send_after(Evict, self(), evict_heap),
   Heap = cache_heap:slip(State#cache.heap),
   case cache_heap:size(Heap) of
      X when X > N ->
         {noreply, State#cache{heap=cache_heap:drop(Heap, State#cache.heir)}};
      _ ->
         {noreply, State#cache{heap=Heap}}
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
cache_put(Key, Val, #cache{heap=Heap}=State) ->
   {_, Head} = cache_heap:head(Heap),
   true = ets:insert(Head, {Key, Val}),
   lists:foreach(
      fun({_, X}) -> ets:delete(X, Key) end, 
      cache_heap:tail(Heap)
   ),
   cache_util:stats(State#cache.stats, {cache, State#cache.name, put}),
   ?DEBUG("cache ~p: put ~p to heap ~p~n", [State#cache.name, Key, Head]),
   State.

cache_put(Key, Val, Expire, #cache{}=State) ->
   Refs = cache_heap:refs(State#cache.heap),
   case lists:splitwith(fun({X, _}) -> X > Expire end, Refs) of
      {[],  _Tail} ->
         cache_put(Key, Val, State);
      {Head, Tail} ->
         [{_, Heap} | Rest] = lists:reverse(Head),
         true = ets:insert(Heap, {Key, Val}),
         lists:foreach(
            fun({_, X}) -> ets:delete(X, Key) end,
            Rest ++ Tail
         ),
         cache_util:stats(State#cache.stats, {cache, State#cache.name, put}),
         ?DEBUG("cache ~p: put ~p to heap ~p~n", [State#cache.name, Key, Heap]),
         State
   end.

%%
%% get cache value
cache_get(Key, #cache{policy=mru}=S) ->
   % cache MRU should not move key anywhere because
   % cache always evicts last generation
   % fall-back to cache lookup
   cache_lookup(Key, S);

cache_get(Key, #cache{}=S) ->
   {_, Head} = cache_heap:head(S#cache.heap),
   case heap_lookup(Key, cache_heap:refs(S#cache.heap)) of
      undefined   ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {Head, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Head]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val;
      {Heap, Val} ->
         true = ets:insert(Head, {Key, Val}),
         _    = ets:delete(Heap, Key),
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, Heap]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end.

%%
%% lookup cache value
cache_lookup(Key, #cache{}=S) ->
   case heap_lookup(Key, cache_heap:refs(S#cache.heap)) of
      undefined    ->
         cache_util:stats(S#cache.stats, {cache, S#cache.name, miss}),
         undefined;
      {_Heap, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [S#cache.name, Key, _Heap]),
         cache_util:stats(S#cache.stats, {cache, S#cache.name, hit}),
         Val
   end.

%%
%% check if key exists
cache_has(Key, #cache{}=S) ->
   case heap_has(Key, cache_heap:refs(S#cache.heap)) of
      false  ->
         false;
      _Heap  ->
         ?DEBUG("cache ~p: has ~p at cell ~p~n", [S#cache.name, Key, _Heap]),
         true
   end.

%%
%% check key ttl
cache_ttl(Key, #cache{}=S) ->
   case heap_has(Key, cache_heap:refs(S#cache.heap)) of
      false       ->
         undefined;
      {Expire, _} ->
         Expire - cache_util:now()
   end.

%%
%%
cache_remove(Key, #cache{}=S) ->
   lists:foreach(
      fun({_, X}) -> ets:delete(X, Key) end,
      cache_heap:refs(S#cache.heap)
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
heap_lookup(Key, [{_, Heap} | Tail]) ->
   case ets:lookup(Heap, Key) of
      []         -> heap_lookup(Key, Tail);
      [{_, Val}] -> {Heap, Val}
   end;

heap_lookup(_Key, []) ->
   undefined.

%%
%%
heap_has(Key, [{_, Heap}=X | Tail]) ->
   case ets:member(Heap, Key) of
      false  -> heap_has(Key, Tail);
      true   -> X
   end;

heap_has(_Key, []) ->
   false.

