%%
-module(cache_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-author('Jose Luis Navarro <jlnavarro@gmail.com>').

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Env = proplists:delete(included_applications, application:get_all_env()),
    {ok,
      {
         {one_for_one, 4, 1800},
         [?CHILD(cache, worker, [Env])]
      }
    }.