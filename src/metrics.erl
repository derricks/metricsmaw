%%% ======================
%%% @doc Top-level interface for interacting with the metrics application
%%% @end
%%% @author Derrick Schneider
%%% =======================

-module(metrics).

-export([add_data/3,get_data/2]).

%% -----------------------
%% @doc Adds data for the given metric of the given type
%%      If the metric isn't in the system already, it will be created
%% @end
%% @spec add_data(MetricName,MetricType,Data) -> ok
%%       where
%%           Data = float()
%% -----------------------
add_data(MetricName,MetricType,Data) ->
	Metric = metric_catalog:find_metric(MetricName,MetricType),
	metric_catalog:update_metric(Metric,Data),
	ok.
	
%%% ----------------------
%%% @doc Fetches the current value of a metric of the given type
%%% @spec get_data(MetricName,MetricType) -> Data
%%% ----------------------
get_data(MetricName,MetricType) ->
	Metric = metric_catalog:find_metric(MetricName,MetricType),
	case Metric of 
		undefined -> undefined;
		_ -> metric_catalog:fetch_value(Metric)
	end.
