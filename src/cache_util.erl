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
%%
-module(cache_util).

-export([
   mdiv/2, madd/2, mmul/2, now/0, stats/2, stats/3, timeout/2
]).

%%
%%
mdiv(undefined, _) ->
   undefined;
mdiv(X, Y) ->
   X div Y.

%%
%%
madd(undefined, _) ->
   undefined;
madd(X, Y) ->
   X + Y.

%%
%%
mmul(undefined, _) ->
   undefined;
mmul(X, Y) ->
   X * Y.


%%
%%
now() ->
   {Mega, Sec, _} = erlang:now(),
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
%%
%%
%% re-set alarm after time
timeout(T, Msg)
 when is_integer(T) ->
   {clock, T, erlang:send_after(T, self(), Msg)};

timeout({clock, T, Timer}, Msg) ->
   erlang:cancel_timer(Timer),
   {clock, T, erlang:send_after(T, self(), Msg)};

timeout(X, _) ->
   X.


