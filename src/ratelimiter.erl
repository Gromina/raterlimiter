-module(ratelimiter).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:start(ratelimiter).

start(_StartType, _StartArgs) ->
    ratelimiter_sup:start_link().

stop(_State) ->
    ok.
