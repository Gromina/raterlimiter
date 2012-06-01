-module(ratelimiter_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% running from command line
start() ->
  application:start(ratelimiter).

%% usual start
start(_StartType, _StartArgs) ->
    ratelimiter_sup:start_link().

stop(_State) ->
    ok.
