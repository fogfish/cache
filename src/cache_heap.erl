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
%%     heap of ETS entities (sorted by age)
-module(cache_heap).

-export([
   new/1, alloc/1, talloc/2, free/1, free/2,
   cells/1, size/1, memory/1, last/1, first/1
]).

-record(heap, {
   cells :: list()      %% list of cells
}).

%%
%% create new heap
new(N) ->
   lists:foldl(
      fun(_, Acc) -> alloc(Acc) end,
      #heap{cells=[]},
      lists:seq(1, N)
   ).

%%
%% allocate new cell
alloc(#heap{}=H) ->
   H#heap{
      cells = [create() | H#heap.cells]
   }.

% tail alloc
talloc(N, #heap{}=H) 
 when length(H#heap.cells) < N ->
   talloc(N,
      H#heap{
         cells = H#heap.cells ++ [create()]
      }
   );
talloc(_, #heap{}=H) ->
   H.

create() ->
   ets:new(undefined, [set, protected]).

%%
%% free cells
free(#heap{cells=Cells}) ->
   [ets:delete(X) || {_, X} <- Cells],
   ok.

free(Cell, #heap{}=H) ->
   ets:delete(Cell),
   H#heap{
      cells = lists:delete(Cell, H#heap.cells)
   }.

%%
%%
cells(#heap{}=H) ->
   H#heap.cells.

%%
%%
size(#heap{}=H) ->
   [ets:info(X, size) || X <- H#heap.cells].

%%
%%
memory(#heap{}=H) ->
   [ets:info(X, memory) || X <- H#heap.cells].

%%
%%
last(#heap{}=H) ->
   lists:last(H#heap.cells).

%%
%%
first(#heap{}=H) ->
   hd(H#heap.cells).
