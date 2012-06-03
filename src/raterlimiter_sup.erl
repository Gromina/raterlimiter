-module(raterlimiter_sup).

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
    ChildSpec = {raterlimiter,                    % id
                 {raterlimiter, start_link, []},  % start params
                 permanent,                       % restart strategy
                 1000,                            % timeout to wait shutdown
                 worker,                          % type of supervised process
                 [raterlimiter]},                 % gen_server module
    {ok, {{one_for_all,5,3600}, [ChildSpec]}}.
