%%% =========================
%%% Unit tests for the metric and gen_metric modules
%%% =========================

-module(metric_tests).

-include_lib("eunit/include/eunit.hrl").

%% -----------------
%% utility methods
%% -----------------
start_metric(MetricType) ->
	{ok,Pid} = metric:start_link(MetricType),
	Pid.
	
%%% -----------------
%%% tests
%%% -----------------
counter_basic_test() ->
	Pid = start_metric(counter),
	metric:add_datum(Pid,{1,1}), % timestamp is ignored
	1 = metric:get_current(Pid),
	metric:add_datum(Pid,{1,1}),
	2 = metric:get_current(Pid).

gauge_basic_test() ->
	Pid = start_metric(gauge),
	metric:add_datum(Pid,{23,1}),
	23 = metric:get_current(Pid).
	
meter_minute_basic_test() ->
	Pid = start_metric(meter_minute),
	metric:add_datum(Pid,50),
	?assert(proplists:get_value(last_minute_avg,metric:get_current(Pid)) == 50).
	
purge_test() ->
	Pid = start_metric(meter_minute),
	metric:add_datum(Pid,1),
	Current = metric:get_current(Pid),
	timer:sleep(2000),
	metric:purge(Pid,1),
	NewCurrent = metric:get_current(Pid),
	?assert(proplists:get_value(last_minute_avg,Current) /= proplists:get_value(last_minute_avg,NewCurrent)).