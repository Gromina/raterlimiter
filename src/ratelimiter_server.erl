-module(ratelimiter_server).
-behaviour(gen_server).

-export([start_link/0]).
%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


%%% INTERFACE
%% the base key is passed from the supervisor. This function
%% should not be called manually.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init(_) ->
    {ok, Timeout} = application:get_env(ratelimiter, timeout),
    {ok, []}. %% [] means state

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.