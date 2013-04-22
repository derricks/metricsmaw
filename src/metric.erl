%%% ====================
%%% @doc Generic server for containing a single metric.
%%%      Metrics are anonymous servers that must be retrieved by metric_catalog
%%% @author Derrick Schneider
%%% @copyright 2013 Derrick Schneider
%%% =====================
-module(metric).
-include("generic_include.hrl").

-define(WAIT_TIME,1000).

-behaviour(gen_server).

%% API
-export([start_link/1,get_current/1,add_datum/2,purge/1]).

%% TODO: Debug/Test only
-ifdef(DEBUG).
-export([purge/2]).
-endif.

%% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).

-record(state,{type,current_data}).


%%% --------------------
%%% API implementations
%%% --------------------
start_link(MetricType) ->
	gen_server:start_link(?MODULE,[MetricType],[]).
	
%% --------------------
%% @doc Gets the current value of the specified metric
%% --------------------
get_current(Pid) ->
	gen_server:call(Pid,get_current,?WAIT_TIME).

%%% -------------------
%%% @doc Adds the relvant datum to the metric
%%% -------------------
add_datum(Pid,Datum) when is_tuple(Datum) ->
	gen_server:cast(Pid,{add,Datum});
add_datum(Pid,Value) when is_number(Value) ->
	add_datum(Pid,{Value,utils:unix_time()}).
	
%%% ------------------
%%% @doc tells the metric to purge old data
%%% ------------------
% TODO: send event (in debug) indicating purge was triggered for testing
purge(Pid) ->
	gen_server:cast(Pid,purge).
	
-ifdef(DEBUG).
%%% -------------------------	
%%% @doc purge data older than the specified time
%%% -------------------------
purge(Pid,ExpireSeconds) ->
	gen_server:cast(Pid,{purge,ExpireSeconds}).
-endif.

%%% --------------------
%%% gen_server implementation
%%% --------------------
init([MetricType]) ->
	{ok,#state{type=MetricType,current_data=MetricType:init()}}.
	
handle_call(get_current,_From,State) ->
	#state{type=MetricType,current_data=Data} = State,
	{reply,MetricType:calculate(Data),State}.
		
handle_cast({add,Datum},State) ->
	#state{type=MetricType,current_data=Data} = State,
	{noreply,State#state{current_data=MetricType:add_data(Datum,Data)}};
handle_cast(purge,State) ->
	#state{type=MetricType,current_data=Data} = State,
	{noreply,State#state{current_data=MetricType:purge(Data)}};
handle_cast({purge,ExpireSeconds},State) ->
	#state{type=MetricType,current_data=Data} = State,
	{noreply,State#state{current_data=MetricType:purge(Data,ExpireSeconds)}}.
	
handle_info(_Info,State) -> 
    {noreply,State}.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
	
terminate(_Reason,_State) -> 
    ok.