-module(raterlimiter_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% usual start
start(_StartType, _StartArgs) ->
    raterlimiter_sup:start_link().

stop(_State) ->
    ok.
