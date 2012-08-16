%% Base behaviour for reporter processes from metricsmaw. Reporters are run on some configurable interval.

% Implementers need to implement an init method that takes a property
%  list of config options; a gather_start method called when it's about to be run; a gather method that takes a MetricName,type and its current data;
%  and a gather_end method called when the run is finished.

-module(gen_reporter).
-export([behaviour_info/1]).

-export([process_init/2]).

behaviour_info(callbacks) ->
	[
	   %% Called when the reporter is first created.
	   %  Arguments
	   %  - Config options
	   %  Return
	   %  - {ok,ready} or {error,Cause}
	   
	   {init,1},
	
	   %% Called before a gathering run. 
	   %  Arguments
	   %  - Config options
	   %  Return
	   %  - An object to be passed in to the first invocation of gather. Good for accumulators.
	   {gather_start,1},
	
	   %% Called for each metric name in the server's list (depending on type)
	   %  Arguments
	   %  - MetricName - the name of the metric
	   %  - MetricType - its type (useful for knowing how to interpret the data)
	   %  - CurrentData - the current state of the metric
	   %  - Gatherer - the object returned from gather_start or the previous invocation of gather
	   %  - Config - the options for this reporter
	   %  Return
	   %  - An object to be passed to the next invocation of gather
	   {gather,5},
	
	   % Called when all the metrics have been parsed
	   % Arguments
	   % - Gatherer, the accumulator created from the previous invocation of gather (or gather_start)
	   % - Config, the config options for this reporter
	   % Return
	   % - {ok,Response}
	   {gather_end,2}
	];
behaviour_info(_) -> undefined.

	
%% Called by the server when this process is started
process_init(Module,Config) ->
	io:format("Starting reporter ~p~n",[Module]),
	{ok,_State} = Module:init(Config),
	loop(Module,undefined).
	
loop(Module,Gatherer) ->
	receive
		{From,{gather_start,Config}} -> 
		    NewGatherer = Module:gather_start(Config),
		    From ! {ok,ready},
		    loop(Module,NewGatherer);
		
		{_From,{gather,MetricName,MetricType,MetricData,Config}} ->
			NewGatherer = Module:gather(MetricName,MetricType,MetricData,Gatherer,Config),
			loop(Module,NewGatherer);
			
		{_From,{gather_end,Config}} ->
			{ok,_Response} = Module:gather_end(Gatherer,Config),
			loop(Module,undefined)
	end.
			
	