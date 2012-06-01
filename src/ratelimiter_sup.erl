
-module(ratelimiter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
    ChildSpec = {ratelimiter,
                 {ratelimiter, start_link, []},
                 permanent,
                 1000,
                 worker,
                 [ratelimiter]},
    {ok, {{one_for_all,5,3600}, [ChildSpec]}}.
