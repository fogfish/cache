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
   new/4
,  size/1
,  head/1
,  tail/1
,  refs/1
,  slip/1
,  drop/2
,  purge/2
]).   

%%
-record(heap, {
   type        = set       :: atom()       %% type of segment
,  ttl         = undefined :: integer()    %% segment expire time
,  cardinality = undefined :: integer()    %% segment cardinality quota
,  memory      = undefined :: integer()    %% segment memory quota
,  segments    = []        :: [integer()]  %% segment references 
}).

%%
%% create new empty heap
-spec(new(atom(), integer(), integer(), integer()) -> #heap{}).

new(Type, TTL, Cardinality, Memory) ->
   init(#heap{
      type        = Type
   ,  ttl         = TTL
   ,  cardinality = Cardinality
   ,  memory      = Memory
   }).

%%
%% return size of heap (number of segments)
-spec(size(#heap{}) -> integer()).

size(#heap{segments=List}) ->
   length(List).

%%
%% return head
-spec(head(#heap{}) -> {integer(), integer()}).

head(#heap{segments=[Head | _]}) ->
   Head.

%%
%% return tail
-spec(tail(#heap{}) -> [{integer(), integer()}]).

tail(#heap{segments=[_ | Tail]}) ->
   Tail.

%%
%% return reference to all segments
-spec(refs(#heap{}) -> [{integer(), integer()}]).

refs(#heap{segments=Refs}) ->
   Refs.

%%
%% slip heap segments and report reason
-spec(slip(#heap{}) -> {ok | ttl | oom | ooc , #heap{}}).

slip(#heap{}=Heap) ->
   case is_expired(cache_util:now(), Heap) of
      false  ->
         {ok, Heap};
      Reason ->
         {Reason, init(Heap)}
   end.

is_expired(Time, #heap{cardinality=C, memory=M, segments=[{Expire, Ref}|_]}) ->
   case 
      {Time >= Expire, ets:info(Ref, size) >= C, ets:info(Ref, memory) >= M}
   of
      {true, _, _} -> ttl;
      {_, true, _} -> ooc;
      {_, _, true} -> oom;
      _            -> false
   end.

%%
%% drop last segment 
-spec(drop(#heap{}, pid()) -> #heap{}).

drop(#heap{segments=Segments}=Heap, Heir) ->
   [{_, Ref}|T] = lists:reverse(Segments),
   free(Heir, Ref),
   Heap#heap{
      segments = lists:reverse(T)
   }.

%%
%% purge cache segments
-spec(purge(#heap{}, pid()) -> #heap{}).

purge(#heap{segments=Segments}=Heap, Heir) ->
   lists:foreach(fun({_, Ref}) -> free(Heir, Ref) end, Segments),
   init(Heap#heap{segments=[]}).


%%
%% create heap segment
init(#heap{segments=Segments}=Heap) ->
   Ref    = ets:new(undefined, [Heap#heap.type, protected]),
   Expire = cache_util:madd(Heap#heap.ttl,  cache_util:now()),
   Heap#heap{segments = [{Expire, Ref} | Segments]}.

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


