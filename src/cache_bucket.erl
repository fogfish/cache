%%
-module(cache_bucket).
-behaviour(gen_server).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([start_link/2]).
-export([put/3, get/2, evict/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_POLICY,     lru).
-define(DEFAULT_TTL,      60000).
-define(DEFAULT_EVICT,    10000).
-define(DEFAULT_CHUNK,      100).

%%
-record(cache, {
   policy,    %%
   memory,    %% memory threshold
   size,      %% size threshold
   chunk,     %% number of evicted elements 

   ttl,       %% cache element ttl
   evict,     %% house keeping timer to evict cache elements
   elements,
   access
}).

%%
%%
start_link(Name, Opts) ->
   gen_server:start_link({local, Name}, ?MODULE, [Opts], []).

init([Opts]) ->
   init(Opts, #cache{
      evict  = ?DEFAULT_EVICT,
      policy = ?DEFAULT_POLICY,
      chunk  = ?DEFAULT_CHUNK,
      ttl    = ?DEFAULT_TTL * 1000
   }).

init([{policy, X} | T], #cache{}=S) ->
   init(T, S#cache{policy=X});

init([{memory, X} | T], #cache{}=S) ->
   init(T, S#cache{memory=X div erlang:system_info(wordsize)});

init([{size,  X} | T], #cache{}=S) ->
   init(T, S#cache{size=X});

init([{chunk, X} | T], #cache{}=S) ->
   init(T, S#cache{chunk=X});

init([{ttl, X} | T], #cache{}=S) ->
   init(T, S#cache{ttl=X * 1000});

init([{evict, X} | T], #cache{}=S) ->
   init(T, S#cache{evict=X});

init([], #cache{evict=Evict}=S) ->
   random:seed(erlang:now()),
   erlang:send_after(Evict, self(), evict),
   {ok,
      S#cache{
         elements = ets:new(undefined, [set, private]),
         access   = ets:new(undefined, [ordered_set, private])
      }
   }.

%%%----------------------------------------------------------------------------   
%%%
%%% api 
%%%
%%%----------------------------------------------------------------------------   

%%
%%
put(Cache, Key, Val) ->
	gen_server:cast(Cache, {put, Key, Val}).

get(Cache, Key) ->
   gen_server:call(Cache, {get, Key}, infinity).

evict(Cache) ->
   gen_server:cast(Cache, evict).

stop(Cache) ->
   gen_server:call(Cache, stop, infinity).

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call({get, Key}, _Tx, #cache{ttl=TTL, elements=E, access=A}=S) ->
   Now = usec(),
   case ets:lookup(E, Key) of
   	[] -> 
   	   {reply, none, S};
   	[{Key, _Val, Expire0}] when Expire0 =< Now ->
   	   {reply, none, S};
   	[{Key, Val, Expire0}] ->
   	   Expire = Now + TTL,
   		ets:insert(E, {Key, Val, Expire}),
   		ets:delete(A, Expire0),
   		ets:insert(A, {Expire, Key}),
   	   {reply, {ok, Val}, S}
   end;

handle_call(stop, _Tx, S) ->
   {stop, normal, ok, S};

handle_call(_Req,  _Tx, S) ->
   {reply, {error, not_implemented}, S}.

%%
%%
handle_cast({put, Key, Val}, #cache{ttl=TTL, elements=E, access=A}=S) ->
   Expire = usec() + TTL,
   ets:insert(E, {Key, Val, Expire}),
   ets:insert(A, {Expire, Key}),
   {noreply, S};

handle_cast(evict, #cache{elements=E, access=A}=S) ->
   evict_expired(usec(), E, A),
   evict_cache(S),
   {noreply, S};

handle_cast(_Req, S) ->
   {noreply, S}.

%%
%%
handle_info(evict, #cache{evict=Evict, elements=E, access=A}=S) ->
   evict_expired(usec(), E, A),
   evict_cache(S),
   erlang:send_after(Evict, self(), evict),
   {noreply, S};


handle_info(_Msg, S) ->
   {noreply, S}.

%%
%%     
terminate(_Reason, _S) ->
   ok.

%%
%% 
code_change(_Vsn, S, _Extra) ->
   {ok, S}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
usec() ->
   {Mega, Sec, USec} = erlang:now(),
   (Mega * 1000000 + Sec) * 1000000 + USec.

%%
%%
check_memory(#cache{memory=undefined}) ->
   true;
check_memory(#cache{memory=Mem, elements=E, access=A}) ->
   Used = ets:info(E, memory) + ets:info(A, memory),
   if
      Used > Mem -> false;
      true       -> true
   end.

%%
%%
check_size(#cache{size=undefined}) ->
   true;
check_size(#cache{size=Size, elements=E}) ->
   Used = ets:info(E, size),
   if
      Used > Size -> false;
      true        -> true
   end.

%%
%%
check(S) ->
   case check_memory(S) of
      true  -> check_size(S);
      false -> false
   end.

%%
%%
evict_expired(Expire, Element, Access) ->
   case ets:first(Access) of
      '$end_of_table' ->
         ok; 
      Time when Time > Expire ->   
         ok;
      Time ->
         [{_, Key}] = ets:lookup(Access, Time),
   	   ets:delete(Element, Key),
   	   ets:delete(Access, Time),
   	   evict_expired(Expire, Element, Access)
  	end.

%%
%%
evict_cache(#cache{policy=lru, chunk=Chunk, elements=E, access=A}=S) ->
   case check(S) of
      true  -> 
         ok;
      false ->
         N = erlang:max(1, random:uniform(Chunk)), 
         evict_lru(N, E, A),
         evict_cache(S)
   end;

evict_cache(#cache{policy=mru, chunk=Chunk, elements=E, access=A}=S) ->
   case check(S) of
      true  -> 
         ok;
      false ->
         N = erlang:max(1, random:uniform(Chunk)), 
         evict_mru(N, E, A),
         evict_cache(S)
   end.


%%
%%
evict_lru(0, _Element, _Access) ->
   ok;
evict_lru(N, Element, Access) ->
   case ets:first(Access) of
      '$end_of_table' ->
         ok; 
      Time ->
         [{_, Key}] = ets:lookup(Access, Time),
         ets:delete(Element, Key),
         ets:delete(Access, Time),
         evict_lru(N - 1, Element, Access)
   end.

%%
%%
evict_mru(0, _Element, _Access) ->
   ok;
evict_mru(N, Element, Access) ->
   case ets:last(Access) of
      '$end_of_table' ->
         ok; 
      Time ->
         [{_, Key}] = ets:lookup(Access, Time),
         ets:delete(Element, Key),
         ets:delete(Access, Time),
         evict_mru(N - 1, Element, Access)
   end.

