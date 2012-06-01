-module(ratelimiter_server).
-behaviour(gen_server).

-include_lib("ratelimiter.hrl").
-export([start_link/0]).

% http://en.wikipedia.org/wiki/Token_bucket

%% Internal Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(ratelimiter, {timeout}).


% TODO
% выяснить правильно ли создаю таблицы (права доступа)


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init(_) ->
    io:format("starting ratelimiter"),
    {ok, Timeout} = application:get_env(ratelimiter, timeout),
    ets:new(?RATELIMITER_TABLE, [named_table, ordered_set, public]),
    timer:send_interval(Timeout, interval),
    {ok, #ratelimiter{timeout=Timeout}}.

handle_call({Client, Scale, Limit}, _From, State) ->
    Result = count_hit(Client, Scale, Limit),
    {reply, Result,  State};

handle_call(_Msg, _From, State) ->
  {reply, ok,  State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% callback called periodically
handle_info(interval, State)->
  io:format("deleting old limiters"),
  remove_old_limiters(State#ratelimiter.timeout),
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
%% Scale of time (1000 bucket every second, 60000 bucket every minute)
%% Limit - max size of bucket
count_hit(Id, Scale, Limit) ->
  Stamp = timestamp(), %% milliseconds
  BucketNumber = trunc(Stamp / Scale), % with scale=1 bucket changes every millisecond
  Key = {BucketNumber, Id},
  case   ets:member(?RATELIMITER_TABLE, Key) of
    false ->
      ets:insert(?RATELIMITER_TABLE, {Key, 1, Stamp, Stamp }),
      {ok, continue};
    true ->
      % increment counter by 1, created_time by 0, and changed_time by current one
      Counters = ets:update_counter(?RATELIMITER_TABLE, Key, [{2,1},{3,0},{4,1,0, Stamp}]),
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
  Matcher = ets:fun2ms(fun ({_,_,Accessed,_}) when Accessed < (NowStamp - Timeout) -> true end),
  ets:select_delete(?RATELIMITER_TABLE, Matcher).

-spec timestamp() -> Timstamp::integer().
%% @doc Returns now() as milliseconds
timestamp() ->
  timestamp(now()).

-spec timestamp({Mega::integer(), Secs::integer(), Micro::integer()}) -> Timestamp::integer().
%% @doc returns Erlang Time as milliseconds
timestamp({Mega, Sec, Micro}) ->
  Mega*1000000*1000 + Sec*1000 + round(Micro/1000).


