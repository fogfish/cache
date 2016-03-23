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

handle_call({put, Key, Val, TTL}, _, State) ->
   {reply, ok, cache_put(Key, Val, TTL, State)};

handle_call({get, Key}, _, State) ->
   {reply, cache_get(Key, State), State};

handle_call({lookup, Key}, _, State) ->
   {reply, cache_lookup(Key, State), State};

handle_call({has, Key}, _, State) ->
   {reply, cache_has(Key, State), State};

handle_call({ttl, Key}, _, State) ->
   {reply, cache_ttl(Key, State), State};

handle_call({remove, Key}, _, State) ->
   {reply, ok, cache_remove(Key, State)};

handle_call({acc, Key, Val}, _, State0) ->
   {Result, State1} = cache_acc(Key, Val, State0),
   {reply, Result, State1};

handle_call({add, Key, Val, TTL}, _, State0) ->
   {Result, State1} = cache_add(Key, Val, TTL, State0),
   {reply, Result, State1};

handle_call({replace, Key, Val, TTL}, _, State0) ->
   {Result, State1} = cache_replace(Key, Val, TTL, State0),
   {reply, Result, State1};

handle_call({prepend, Key, Val}, _, State0) ->
   {Result, State1} = cache_prepend(Key, Val, State0),
   {reply, Result, State1};

handle_call({append, Key, Val}, _, State0) ->
   {Result, State1} = cache_append(Key, Val, State0),
   {reply, Result, State1};

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

handle_call(_, _, State) ->
   {noreply, State}.

%%
%%
handle_cast({put, Key, Val, TTL}, State) ->
   {noreply, cache_put(Key, Val, TTL, State)};

handle_cast({remove, Key}, State) ->
   {noreply, cache_remove(Key, State)};

handle_cast({acc, Key, Val}, State0) ->
   {_, State1} = cache_acc(Key, Val, State0),
   {noreply, State1};

handle_cast({add, Key, Val, TTL}, State0) ->
   {_, State1} = cache_add(Key, Val, TTL, State0),
   {noreply, State1};

handle_cast({replace, Key, Val, TTL}, State0) ->
   {_, State1} = cache_replace(Key, Val, TTL, State0),
   {noreply, State1};

handle_cast({prepend, Key, Val}, State0) ->
   {_, State1} = cache_prepend(Key, Val, State0),
   {noreply, State1};

handle_cast({append, Key, Val}, State0) ->
   {_, State1} = cache_append(Key, Val, State0),
   {noreply, State1};

handle_cast(_, State) ->
   {noreply, State}.

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
cache_put(Key, Val, undefined, #cache{name=_Name, heap=Heap}=State) ->
   {_, Head} = cache_heap:head(Heap),
   true = ets:insert(Head, {Key, Val}),
   ok   = heap_remove(Key, cache_heap:tail(Heap)),
   _    = stats(put, State),
   ?DEBUG("cache ~p: put ~p to heap ~p~n", [_Name, Key, Head]),
   State;

cache_put(Key, Val, TTL, #cache{name=_Name, heap=Heap}=State) ->
   Expire = cache_util:now() + TTL,
   Refs   = cache_heap:refs(Heap),
   case lists:splitwith(fun({X, _}) -> X > Expire end, Refs) of
      {[],  _Tail} ->
         cache_put(Key, Val, undefined, State);
      {Head, Tail} ->
         [{_, Inst} | Rest] = lists:reverse(Head),
         true = ets:insert(Inst, {Key, Val}),
         ok   = heap_remove(Key, Rest ++ Tail),
         _    = stats(put, State),
         ?DEBUG("cache ~p: put ~p to heap ~p~n", [_Name, Key, Inst]),
         State
   end.

%%
%% get cache value
cache_get(Key, #cache{policy=mru}=State) ->
   % cache MRU should not move key anywhere because
   % cache always evicts last generation
   % fall-back to cache lookup
   cache_lookup(Key, State);

cache_get(Key, #cache{name=_Name, heap=Heap}=State) ->
   {_, Head} = cache_heap:head(Heap),
   case heap_lookup(Key, cache_heap:refs(Heap)) of
      undefined   ->
         stats(miss, State),
         undefined;
      {Head, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [_Name, Key, Head]),
         stats(hit, State),
         Val;
      {Cell, Val} ->
         true = ets:insert(Head, {Key, Val}),
         _    = ets:delete(Cell, Key),
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [_Name, Key, Cell]),
         stats(hit, State),
         Val
   end.

%%
%% lookup cache value
cache_lookup(Key, #cache{name=_Name, heap=Heap}=State) ->
   case heap_lookup(Key, cache_heap:refs(Heap)) of
      undefined    ->
         stats(miss, State),
         undefined;
      {_Cell, Val} ->
         ?DEBUG("cache ~p: get ~p at cell ~p~n", [_Name, Key, _Cell]),
         stats(hit,  State),
         Val
   end.

%%
%% check if key exists
cache_has(Key, #cache{name=_Name, heap=Heap}) ->
   case heap_has(Key, cache_heap:refs(Heap)) of
      false  ->
         false;
      _Heap  ->
         ?DEBUG("cache ~p: has ~p at cell ~p~n", [_Name, Key, _Heap]),
         true
   end.

%%
%% check key ttl
cache_ttl(Key,#cache{heap=Heap}) ->
   case heap_has(Key, cache_heap:refs(Heap)) of
      false       ->
         undefined;
      {Expire, _} ->
         Expire - cache_util:now()
   end.

%%
%%
cache_remove(Key, #cache{name=_Name, heap=Heap}=State) ->
   ok = heap_remove(Key, cache_heap:refs(Heap)),
   _  = stats(remove, State),
   ?DEBUG("cache ~p: remove ~p~n", [_Name, Key]),
   State.

%%
%% @todo: reduce one write
cache_acc(Key, Val, State)
 when is_integer(Val) ->
   case cache_get(Key, State) of
      undefined ->
         {undefined, cache_put(Key, Val, undefined, State)};
      X when is_integer(X) ->
         {X, cache_put(Key, X + Val, undefined, State)};
      X when is_tuple(X) ->
         {erlang:element(1, X), cache_put(Key, tuple_acc({1, Val}, X), undefined, State)};
      _  ->
         {badarg, State}
   end;
cache_acc(Key, Val, State) ->
   case cache_get(Key, State) of
      X when is_tuple(X) ->
         {X, cache_put(Key, tuple_acc(Val, X), undefined, State)};
      _  ->
         {badarg, State}
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
cache_add(Key, Val, TTL, State) ->
   case cache_has(Key, State) of
      true  ->
         {{error, conflict}, State};
      false ->
         {ok, cache_put(Key, Val, TTL, State)}
   end.

%%
%%
cache_replace(Key, Val, TTL, State) ->
   case cache_has(Key, State) of
      true  ->
         {ok, cache_put(Key, Val, TTL, State)};
      false ->
         {{error, not_found}, State}
   end.

%%
%%
cache_prepend(Key, Val, State) ->
   % @todo: reduce one write
   case cache_get(Key, State) of
      undefined  ->
         {ok, cache_put(Key, [Val], undefined, State)};
      X when is_list(X) ->
         {ok, cache_put(Key, [Val|X], undefined, State)};
      X ->
         {ok, cache_put(Key, [Val,X], undefined, State)}
   end.

%%
%%
cache_append(Key, Val, State) ->
   % @todo: reduce one write
   case cache_get(Key, State) of
      undefined  ->
         {ok, cache_put(Key, [Val], undefined, State)};
      X when is_list(X) ->
         {ok, cache_put(Key, X++[Val], undefined, State)};
      X ->
         {ok, cache_put(Key, [X, Val], undefined, State)}
   end.


%%
%% remove key from heap segments
heap_remove(Key, Heap) ->
   lists:foreach(
      fun({_, Id}) -> ets:delete(Id, Key) end, 
      Heap
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

%%
%% update statistic
stats(_, #cache{stats = undefined}) ->
   ok;
stats(X, #cache{stats = Stats, name = Name}) ->
   cache_util:stats(Stats, {cache, Name, X}).

