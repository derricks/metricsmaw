-module(console_reporter).

-behaviour(gen_reporter).

-export([init/1,gather_start/1,gather/5,gather_end/2]).

init(_Config) -> {ok,ready}.

gather_start(_Config) -> [].

gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_float(CurrentData) ->
	[MetricName ++ "	" ++ float_to_list(CurrentData) ++ "\n"| Gatherer];
gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_integer(CurrentData) ->
	[MetricName ++ "	" ++ integer_to_list(CurrentData) ++ "\n"| Gatherer];
gather(MetricName,_MetricType,CurrentData,Gatherer,_Config) when is_list(CurrentData) ->
	[MetricName ++ "	" ++ utils:join([Value || {_Key,Value} <- CurrentData],"	") ++ "\n"|Gatherer].
	
gather_end([],_Config) -> {ok,[]};
gather_end(Gatherer,_Config) ->
	io:format("~s",[list_to_binary(Gatherer)]),
	{ok,[]}.
