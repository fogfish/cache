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
%%   cache utility
-module(cache_util).

-export([
   mdiv/2, madd/2, mmul/2, now/0, stats/2, stats/3, timeout/2
]).

%%
%% maybe div
mdiv(X, Y)
 when X =:= undefined orelse Y =:= undefined ->
   undefined;
mdiv(X, Y) ->
   X div Y.

%%
%% maybe add
madd(X, Y)
 when X =:= undefined orelse Y =:= undefined ->
   undefined;
madd(X, Y) ->
   X + Y.

%%
%% maybe multiply
mmul(X, Y)
 when X =:= undefined orelse Y =:= undefined ->
   undefined;
mmul(X, Y) ->
   X * Y.

%%
%%
now() ->
   {Mega, Sec, _} = os:timestamp(),
   Mega * 1000000 + Sec.   

%%
%% 
stats(undefined,    _) ->
   ok;
stats({M, F}, Counter) ->
   M:F(Counter);
stats(Fun,    Counter)
 when is_function(Fun) ->
   Fun(Counter).

stats(undefined,    _,   _) ->
   ok;
stats({M, F}, Counter, Val) ->
   M:F(Counter, Val);
stats(Fun,    Counter, Val)
 when is_function(Fun) ->
   Fun(Counter, Val).

%%
%% set / reset timeout
timeout(T, Msg)
 when is_integer(T) ->
   {clock, T, erlang:send_after(T, self(), Msg)};

timeout({clock, T, Timer}, Msg) ->
   erlang:cancel_timer(Timer),
   {clock, T, erlang:send_after(T, self(), Msg)};

timeout(X, _) ->
   X.


