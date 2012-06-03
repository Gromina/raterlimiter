-module(raterlimiter_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% running from command line
start() ->
  application:start(raterlimiter).

%% usual start
start(_StartType, _StartArgs) ->
    raterlimiter_sup:start_link().

stop(_State) ->
    ok.
