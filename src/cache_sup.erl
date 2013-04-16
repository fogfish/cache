%%
-module(cache_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   lists:foreach(fun default_cache/1, application:get_all_env(cache)),
   {ok, Sup}.
   
init([]) -> 
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

default_cache({Name, Opts}) ->
   supervisor:start_child(?MODULE, {
      Name,
      {cache, start_link, [Name, Opts]},
      permanent, 900000, worker, dynamic
   }).