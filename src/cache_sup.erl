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
%%    root cache supervisor
-module(cache_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start_link/0, init/1]).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   lists:foreach(
      fun default_cache/1,
      proplists:delete(included_applications, application:get_all_env())
   ),
   {ok, Sup}.
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%% create default cache
default_cache({Name, Opts}) ->
   {ok, _} = supervisor:start_child(?MODULE, {
      Name,
      {cache, start_link, [Name, Opts]},
      permanent, 900000, worker, dynamic
   }).
