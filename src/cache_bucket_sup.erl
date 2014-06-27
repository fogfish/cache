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
-module(cache_bucket_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-author('Jose Luis Navarro <jlnavarro@gmail.com>').

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok,
      {
         {one_for_one, 4, 1800},
         [?CHILD(worker, Name, cache, [Name, Opts]) || {Name, Opts} <- default_cache()]
      }
   }.

%%
%% list of default cache specification
default_cache() ->
   proplists:delete(included_applications, application:get_all_env()).
