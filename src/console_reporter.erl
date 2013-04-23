-module(console_reporter).

-behaviour(gen_reporter).

-export([init/1,gather_start/1,gather/5,gather_end/2]).

init(_Config) -> {ok,ready}.

gather_start(_Config) -> [].

gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_float(CurrentData) ->
	[MetricName ++ "	" ++ float_to_list(CurrentData) ++ utils:newline()| Gatherer];
gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_integer(CurrentData) ->
	[MetricName ++ "	" ++ integer_to_list(CurrentData) ++ utils:newline() | Gatherer];
gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_list(CurrentData) ->
	[MetricName ++ "	" ++ utils:join([Value || {_Key,Value} <- CurrentData],"	") ++ utils:newline()|Gatherer].
	
gather_end([],_Config) -> io:format("No metrics data~n"),{ok,[]};
gather_end(Gatherer,_Config) ->
	io:format("~p~n",[Gatherer]),
	{ok,[]}.