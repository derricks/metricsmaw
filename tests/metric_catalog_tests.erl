%%% =================
%%% @doc Unit tests for metric_catalog, the interface for getting to metrics objects
%%% =================

-module(metric_catalog_tests).

-include_lib("eunit/include/eunit.hrl").


metrics_finding_test_() ->
      {setup,fun start_servers/0,
		[{"Metric Catalog returns same pid",?_assert(metric_catalog:find_metric(test,counter) =:= metric_catalog:find_metric(test,counter))},
		 {"Invalid metrics types return error messages",?_assert({invalid_type,ljljk} =:= metric_catalog:find_metric(test,ljljk))}
		]}.
	
start_servers() ->
	{ok,_CatalogServerPid} = metric_catalog:start_link(),
	{ok,_MetricSupPid} = metric_sup:start_link(),
	ok.
