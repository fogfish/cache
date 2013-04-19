

%-define(VERBOSE, true).
-ifdef(VERBOSE).
   -define(DEBUG(Str, Args), error_logger:error_msg(Str, Args)).
-else.
   -define(DEBUG(Str, Args), ok).
-endif.


%% default cache eviction policy
-define(DEF_CACHE_POLICY, lru).

%% default cache ttl and number of generations
-define(DEF_CACHE_TTL,   600).
-define(DEF_CACHE_N,      10).

%% default cache house keeping frequency
-define(DEF_CACHE_QUOTA,   5). 
