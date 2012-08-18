-module(graphite_reporter).

% Metrics reporter 

-behaviour(gen_reporter).

-export([init/1,gather_start/1,gather/5,gather_end/2]).


init(_Config) -> {ok,ready}.

gather_start(_) -> [].

gather(MetricName,_MetricType,Data,Gatherer,_Config) when is_float(Data); is_integer(Data) ->
	[utils:join(["metricsmaw." ++ MetricName,utils:data_to_string(Data),integer_to_list(utils:unix_time()),"\n"]," ") | Gatherer];
	
%% In this case, create a single line for each element in the list, which is assumed to be a list of {Key,Value} tuples
gather(MetricName,MetricType,Data,Gatherer,Config) when is_list(Data) ->
	SplitData = lists:foldl(
	
	     % just use the primitive form of this function to construct the string
	   fun({DataKey,DataValue}) -> gather(MetricName ++ "." ++ atom_to_list(DataKey), MetricType, DataValue, [], Config) end,
	   [],
	   Data),
	[SplitData|Gatherer].
	
% no metrics = no need to send
gather_end([],_Config) -> {ok,[]};
gather_end(Gatherer,Config) ->
	% ship it off to the graphite host
	io:format("Sending Msg to Graphite ~p~n",[lists:flatten(Gatherer)]),
	GraphiteHost = proplists:get_value(host,Config,"localhost"),
	GraphitePort = proplists:get_value(port,Config,2003),
	Timeout = proplists:get_value(timeout,Config,6000),
	Response = gen_tcp:connect(GraphiteHost,GraphitePort,[list,{packet,0}],Timeout),
	case Response of
		{ok,Socket} ->
			gen_tcp:send(Socket,lists:flatten(Gatherer)),
			gen_tcp:close(Socket);
		{error,Reason} ->
			io:format("Could not send to graphite ~p~n",[Reason])
	end,
	{ok,[]}.
	

