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
%% @description
%%   cache segmented heap
-module(cache_heap).

-export([
   new/5
,  refs/1
,  split/1
,  split/2
,  slip/2
,  purge/2
]).   

%%
-type segment() :: [{integer(), reference()}].
-type heir()    :: undefined | atom() | pid().
-type queue()   :: {[segment()], [segment()]}.

-record(heap, {
   type        = set       :: atom()       %% type of segment
,  n           = undefined :: integer()    %% number of segments
,  ttl         = undefined :: integer()    %% segment expire time
,  cardinality = undefined :: integer()    %% segment cardinality quota
,  memory      = undefined :: integer()    %% segment memory quota
,  segments    = undefined :: queue()      %% segment references
}).

%%
%% create new empty heap
-spec new(atom(), integer(), integer(), integer(), integer()) -> #heap{}.

new(Type, N, TTL, Cardinality, Memory) ->
   init(#heap{
      type        = Type
   ,  n           = N
   ,  ttl         = TTL div N
   ,  cardinality = cache_util:mdiv(Cardinality, N)
   ,  memory      = cache_util:mdiv(cache_util:mdiv(Memory,  N), erlang:system_info(wordsize))
   }).

init(#heap{type = Type, n = N, ttl = TTL} = Heap) ->
   T = cache_util:now(),
   Segments = lists:map(
      fun(Expire) ->
         Ref = ets:new(undefined, [Type, protected]),
         {T + Expire, Ref}
      end,
      lists:seq(TTL, N * TTL, TTL)
   ),
   Heap#heap{segments = queue:from_list(Segments)}.

%%
%%
-spec refs(#heap{}) -> [segment()].

refs(#heap{segments = Segments}) ->
   lists:reverse(queue:to_list(Segments)).

%%
%% split heap to writable segment and others
-spec split(#heap{}) -> {segment(), queue()}.

split(#heap{segments = Segments}) ->
   {{value, Head}, Tail} = queue:out_r(Segments),
   {Head, Tail}.


-spec split(_, #heap{}) -> {segment(), queue()}.

split(Expire, #heap{segments = {Tail, Head}} = Heap) ->
   case 
      lists:splitwith(
         fun({T, _}) -> T > Expire end,
         Tail
      )
   of
      {_, []} ->
         case
            lists:splitwith(
               fun({T, _}) -> T < Expire end,
               Head
            )
         of
            {[Segment | A], []} ->
               {Segment, {Tail, A}};
            {A, [Segment | B]} -> 
               {Segment, {Tail, A ++ B}}
         end;
      {[], _} ->
         split(Heap);
      {A,  B} ->
         [Segment | Ax] = lists:reverse(A),
         {Segment, {lists:reverse(Ax) ++ B, Head}}
   end.


%%
%% slip heap segments and report reason
-spec slip(heir(), #heap{}) -> {ok | ttl | oom | ooc , #heap{}}.

slip(Heir, #heap{} = Heap) ->
   case is_expired(cache_util:now(), Heap) of
      false  ->
         {ok, Heap};
      Reason ->
         {Reason,
            heap_remove_segment(Heir, heap_create_segment(Heap))
         }
   end.

is_expired(Time, Heap) ->
   case is_expired_tail(Time, Heap) of
      false  ->
         is_expired_head(Heap);
      Return ->
         Return
   end.

is_expired_tail(Time, #heap{segments = Segments}) ->
   {Expire, _} = queue:head(Segments),
   case Time >= Expire of
      true ->
         ttl;
      false ->
         false
   end.

is_expired_head(#heap{cardinality = Card, memory = Mem, segments = Segments}) ->
   {_, Ref} = queue:last(Segments),
   case 
      {ets:info(Ref, size) >= Card, ets:info(Ref, memory) >= Mem}
   of
      {true, _} -> ooc;
      {_, true} -> oom;
      _         -> false
   end.

heap_create_segment(#heap{type = Type, ttl = TTL, segments = Segments} = Heap) ->
   {LastTTL, _} = queue:last(Segments),
   Ref    = ets:new(undefined, [Type, protected]),
   Expire = TTL + LastTTL,
   Heap#heap{segments = queue:in({Expire, Ref}, Segments)}.

heap_remove_segment(Heir, #heap{segments = Segments} = Heap) ->
   {{value, {_, Ref}}, T} = queue:out(Segments),
   true = free(Heir, Ref),
   Heap#heap{segments = T}.

%%
%% purge cache segments
-spec purge(heir(), #heap{}) -> #heap{}.

purge(Heir, #heap{segments = Segments} = Heap) ->
   lists:foreach(
      fun({_, Ref}) -> true = free(Heir, Ref) end, 
      queue:to_list(Segments)
   ),
   init(Heap#heap{segments = undefined}).

%%
%% destroy heap segment
free(undefined, Ref) ->
   ets:delete(Ref);

free(Heir, Ref)
 when is_pid(Heir) ->
   case erlang:is_process_alive(Heir) of
      true  ->
         ets:give_away(Ref, Heir, evicted);
      false ->
         ets:delete(Ref)
   end;

free(Heir, Ref)
 when is_atom(Heir) ->
   case erlang:whereis(Heir) of
      undefined ->
         ets:delete(Ref);
      Pid       ->
         ets:give_away(Ref, Pid, evicted)
   end.


