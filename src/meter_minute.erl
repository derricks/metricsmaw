%% A metric that tracks rate by minute. It tracks average over last minute, average over last 5 minutes, average over 15 minutes
%  and overall average

% is it enough to calculate the average rate over the relevant timespans?
% say I have the following data points
% 3:30 = 345
% 3:30 = 123
% 3:33 = 756
% rate for 3:30 = 468
% rate for 3:33 = 756
% average rate over 5 minutes is 1224 / 2 = 612 (vs. 1224/3 = 408)
% so one has to bucket values per minute and then average those rates to get the 5/15 rates.
-module(meter_minute).
-define (ITEM_AGE_SECONDS, 900).
-define (MINUTE_SECONDS, 60).
-behaviour(gen_metric).

%% API
% @todo: Make this debug/test only, since it's only used for testing purposes.
-export([purge/2]).

-export([init/0,add_data/2,type/0,reset/0,purge/1,calculate/1]).

type() -> meter_minute.

init() -> [].

add_data(Datum,State) -> [Datum|State].

reset() -> [].

purge(State) -> 
   % remove items older than ITEM_AGE_SECONDS
   data_records_within_timerange(State,0,?ITEM_AGE_SECONDS).

purge(State,ExpireSeconds) ->
   data_records_within_timerange(State,0,ExpireSeconds).

calculate(State) ->
	% construct a list of tuples of the following
	% {last_minute_avg,Data}
	% {last_five_minutes_avg,Data}
	% {last_fifteen_minutes_avg,Data}
	% TODO - probably can construct all three lists with one pass-through of the data, which could be useful if there's a lot of data
    [{last_minute_avg,avg_minute_rate_for_data(State,?MINUTE_SECONDS)},
     {last_five_minutes_avg,avg_minute_rate_for_data(State,5 * ?MINUTE_SECONDS)},
     {last_fifteen_minutes_avg,avg_minute_rate_for_data(State,15 * ?MINUTE_SECONDS)}
    ].
	
% calculates the average per-minute rate for the last N seconds	
avg_minute_rate_for_data(DataRecords,UpToNSeconds) ->
	% trim the list to just the ones we care about
	avg_minute_rate_for_data(data_records_within_timerange(DataRecords,0,UpToNSeconds),0,0,UpToNSeconds).
	
% todo: this is highly inefficient, filtering the records over and over again
% is there a way to build up all the 60-second intervals between 0 and Seconds and then filter into the appropriate bucket?
% perhaps with proplists?
avg_minute_rate_for_data(_DataRecords,CurrentSum,CurrentCount,0) -> CurrentSum / CurrentCount;
avg_minute_rate_for_data(DataRecords,CurrentSum,CurrentCount,SecondsBack) ->
	
	  % find the points within this range
	  DataPointsInRange = data_records_within_timerange(DataRecords,SecondsBack - ?MINUTE_SECONDS,SecondsBack),
	  DataSum = lists:foldl(
	    fun({Data,_Timestamp},CurAcc) ->
		     CurAcc + Data
	    end   
	    ,0
	    ,DataPointsInRange
	  	),
	
	  % and now call into the next piece of the loop
	  avg_minute_rate_for_data(DataRecords,DataSum + CurrentSum, CurrentCount + 1, SecondsBack - ?MINUTE_SECONDS).
			
% within a set of records of the form {Data,Timestamp}, filter out those between Low and High seconds and keep that
data_records_within_timerange(DataRecords,LowSeconds,HighSeconds) ->
	 CurTime = utils:unix_time(),
	 lists:filter(
	    fun({_Data,DataTs}) ->
		     CurTime - DataTs =< HighSeconds andalso CurTime - DataTs >= LowSeconds
		end
	 	,DataRecords   
	 ).
	
