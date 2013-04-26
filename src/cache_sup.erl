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
%%    root cache supervisor
-module(cache_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   lists:foreach(fun default_cache/1, default()),
   {ok, Sup}.
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%% create default caches
default() ->
   case application:get_env(cache, default) of
      undefined -> [];
      {ok, Val} -> Val
   end.

default_cache({Name, Opts}) ->
   {ok, _} = supervisor:start_child(?MODULE, {
      Name,
      {cache, start_link, [Name, Opts]},
      permanent, 900000, worker, dynamic
   }).
