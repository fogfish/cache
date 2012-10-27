%%
-module(cache_app).
-behaviour(application).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start/2, stop/1]).

start(_Type, _Args) -> 
   cache_sup:start_link(). 

stop(_State) ->
        ok.