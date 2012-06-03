-module(raterlimiter).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").
-export([start_link/0]).

% http://en.wikipedia.org/wiki/Token_bucket

%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(RATERLIMITER_TABLE, raterlimiter_buckets).

-record(raterlimiter, {cleanup_rate, timeout}).
%% todo
%% -->  raterlimiter


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init(_) ->
    {ok, Timeout} = application:get_env(raterlimiter, timeout),
    {ok, Rate} = application:get_env(raterlimiter, cleanup_rate),
    io:format("Starting Raterlimiter with Timeout ~p, Cleanup every ~p milliseconds ~n",[Timeout, Rate]),

    ets:new(?RATERLIMITER_TABLE, [named_table, ordered_set, private]),

    timer:send_interval(Rate, interval),
    {ok, #raterlimiter{timeout=Timeout, cleanup_rate=Rate}}.

handle_call({Client, Scale, Limit}, _From, State) ->
    Result = count_hit(Client, Scale, Limit),
    {reply, Result,  State};

handle_call(_Msg, _From, State) ->
  {reply, ok,  State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% callback called periodically
handle_info(interval, State)->
  remove_old_limiters(State#raterlimiter.timeout),
  {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

% internal calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec count_hit(Id::binary(), Scale::integer(), Limit::integer()) -> {ok, continue} | {fail, Count::integer()}.
%% @doc Counts request by ID and blocks it if rate limiter fires
%% ID of the client
%% Scale of time (1000 = new bucket every second, 60000 = bucket every minute)
%% Limit - max size of bucket
count_hit(Id, Scale, Limit) ->
  Stamp = timestamp(),                    %% milliseconds since 00:00 GMT, January 1, 1970
  BucketNumber = trunc(Stamp / Scale),    %% with scale=1 bucket changes every millisecond
  Key = {BucketNumber, Id},
  case   ets:member(?RATERLIMITER_TABLE, Key) of
    false ->
      ets:insert(?RATERLIMITER_TABLE, {Key, 1, Stamp, Stamp }),
      {ok, continue};
    true ->
      % increment counter by 1, created_time by 0, and changed_time by current one
      Counters = ets:update_counter(?RATERLIMITER_TABLE, Key, [{2,1},{3,0},{4,1,0, Stamp}]),
      % Counter, created at, changed at
      [BucketSize, _, _] = Counters,
      if
        (BucketSize > Limit) ->
                {fail, Limit};
        true ->
                {ok, continue}
      end
    end.

-spec remove_old_limiters(Timeout::integer()) -> Number::integer().
%% @doc Removes old counters and returns number of deleted counters.
remove_old_limiters(Timeout) ->
  NowStamp = timestamp(),
  Matcher = ets:fun2ms(fun ({_,_,_,Accessed}) when Accessed < (NowStamp - Timeout) -> true end),
  ets:select_delete(?RATERLIMITER_TABLE, Matcher).

-spec timestamp() -> Timstamp::integer().
%% @doc Returns now() as milliseconds
timestamp() ->
  timestamp(now()).

-spec timestamp({Mega::integer(), Secs::integer(), Micro::integer()}) -> Timestamp::integer().
%% @doc returns Erlang Time as milliseconds
timestamp({Mega, Sec, Micro}) ->
  1000*(Mega*1000000 + Sec) + round(Micro/1000).


