%%% ===========================
%%% gen_server implementation to handle administrative tasks for metrics, such as purging old data
%%% intended to be a sibling of metric_catalog 
%%% @author Derrick Schneider
%%% @copyright 2013 Derrick Schneider
%%% ============================

-module(metric_custodian).
-define(SERVER,?MODULE).
-define(PURGE_RATE, 300000).

-behaviour(gen_server).

-export([start_link/0,purge_metrics/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

%%% ---------------------------
%%% API
%%% ---------------------------
start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).
	

%%% ---------------------------
%%% gen_server implementation
%%% ---------------------------
init([]) ->
	start_metrics_purge_timer(),
	{ok,unedfined}.
	
handle_call(_Request,_From,State) ->
	{reply,ok,State}.
	
handle_cast(_Request,State) ->
	{noreply,State}.
	
handle_info(_Request,State) ->
	{noreply,State}.
	
terminate(_Reason,_State) ->
	ok.
	
code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	

%%% ---------------------------
%%% privates
%%% ---------------------------

%%% ---------------------------
%%% @doc walks
purge_metrics() ->
	metric_catalog:foreach(fun(_MatricKey,MetricPid) -> metric:purge(MetricPid) end),
	start_metrics_purge_timer().
	
start_metrics_purge_timer() ->
	timer:apply_after(?PURGE_RATE,?MODULE,purge_metrics,[]).
	
