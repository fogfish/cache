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

%%
%% define default select limit
-define(CONFIG_SELECT,   1000).


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

%% default cache i/o timeout
-define(DEF_CACHE_TIMEOUT, 60000).
