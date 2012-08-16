%% Reporter for dumping into a CSV file

-module(csv_reporter).
-behaviour(gen_reporter).
-export([init/1,gather_start/1,gather/5,gather_end/2]).

dir_name(Config) ->
	proplists:get_value(directory,Config,".").
	
%% make sure directory exists
init(Config) ->
	filelib:ensure_dir(dir_name(Config)),
	{ok,ready}.
	
gather_start(_Config) -> {ok,ready}.


gather(MetricName,MetricType,CurrentData,_Gatherer,Config) ->
	FileName = dir_name(Config) ++ "/" ++ MetricName ++ "-" ++ atom_to_list(MetricType) ++ ".csv",
	{ResponseCode,Response} = file:open(FileName,[append]),
	case ResponseCode of
		ok -> 
			write_to_file(Response,MetricName,CurrentData),
			file:close(Response);
		error ->
			io:format("Could not open file for writing ~p~n",[Response])
	end.
	
gather_end(_Gatherer,_Config) -> {ok,ready}.

% simple case
write_to_file(IoDevice,MetricName,Data) when is_integer(Data);is_float(Data) ->
	io:format(IoDevice,"~s,~w~n",[list_to_binary(MetricName),Data]);
	
write_to_file(IoDevice,MetricName,Data) when is_list(Data) ->
	% Data is assumed to be a proplist
	% create a list (to be turned into a string) of all the values in Data
	ValueString = utils:join([Value || {_Key,Value} <- Data],","),
	io:format(IoDevice,"~s,~s~n",[list_to_binary(MetricName),list_to_binary(ValueString)]).