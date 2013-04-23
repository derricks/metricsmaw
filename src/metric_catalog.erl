%%% =====================
%%% @doc Catalog of metrics objects defined in the system.
%%%      This wraps a process dictionary in a gen_server implementation
%%%      Intended to be a child of metricsmaw, and is the way to get to specific metric processes
%%% @author Derrick Schneider
%%% @copyright 2013 Derrick Schneider
%%% =====================

-module(metric_catalog).

-define(SERVER,?MODULE).
-define(DEFAULT_TIMEOUT,20000).
-define(TABLE_NAME,metric_to_pid).
-define(METRIC_TYPES,[counter,gauge,meter_minute]).

-behaviour(gen_server).

%%% API
-export([start_link/0,find_metric/2,update_metric/2,fetch_value/1,foreach/1]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link() ->
	gen_server:start_link({local,?SERVER},?MODULE,[],[]).
	
%%% ---------------------
%%% @doc Find or create the given metric of the given type
%%% 
%%% ---------------------
find_metric(MetricName,MetricType) ->
	gen_server:call(?SERVER,{find,{MetricName,MetricType}},?DEFAULT_TIMEOUT).
	
%%% ----------------------------
%%% @doc For a given pid (acquired via find_metric, update its current value)
%%%      If data is a tuplie with a timestamp at the end, that is used as the timestamp
%%%      Otherwise, the current time when this method is invoked is used
%%% ----------------------------
update_metric(MetricPid,{_Datum,_Timestamp}=Data) ->
	metric:add_datum(MetricPid,Data);
update_metric(MetricPid,Data) when is_number(Data) -> % when it's just a number
	update_metric(MetricPid,{Data,utils:unix_time()}).

%%% -----------------------------
%%% @doc Given a metric pid, acquired from find_metric, return its current value
%%% -----------------------------
fetch_value(MetricPid) ->
	metric:get_current(MetricPid).
		
%%% -----------------------------
%%% @doc Execute Fun for each registered metric
%%% -----------------------------
foreach(Fun) ->
	%done as a call to ensure that any clients wait for the whole thing to finish before doing other steps
	gen_server:call(?SERVER,{foreach,Fun}).

%%% -----------------------
%%% gen_server implementations
%%% ----------------------
init(_Args) ->
	Table = ets:new(?TABLE_NAME,[named_table,{read_concurrency,true}]),
	{ok,Table}.
	
handle_call({find,{MetricName,MetricType}},_From,Table) ->
	case is_valid_metric_type(MetricType) of
		true ->
		% lookup the existin PID for this metric tuple	
			case ets:match(Table,{{MetricName,MetricType},'$1'}) of
				[[Found]] -> {reply,Found,Table};
				% start a new one
				[]      -> {ok,Pid} = metric_sup:start_child(MetricType),
				           ets:insert(Table,{{MetricName,MetricType},Pid}),
				           {reply,Pid,Table}
			end;
		false -> {reply,{invalid_type,MetricType},Table}
	end;
handle_call({foreach,Fun},_From,Table) ->
	FirstKey = ets:first(Table),
	process_metric_and_advance(Fun,FirstKey),
	{reply,ok,Table};
handle_call(_Request,_From,Table) ->
	{reply,ok,Table}.
	
handle_cast(_Request,Table) ->
	{noreply,Table}.
	
handle_info(_Info,Table) ->
	{noreply,Table}.
	
terminate(_Reason,Table) ->
	ets:delete(Table).
	
code_change(_OldVsn,Table,_Extra) ->
	{ok,Table}.
	
%%% ============================
%%% privates
%%% ============================

%%% ----------------------------
%%% @doc Determine whether the passed-in value is a valid metric type
%%% ----------------------------
is_valid_metric_type(MetricType) ->
	lists:any(fun(Item) -> MetricType =:= Item end,?METRIC_TYPES).

process_metric_and_advance(_Fun,'$end_of_table') -> ok;
process_metric_and_advance(Fun,Key) ->
	case ets:lookup(?TABLE_NAME,Key) of
   	    [{Key,Pid}] ->
			Fun(Key,Pid);
		[] ->
			io:format("Key not found ~p~n",[Key]) % would be odd, but ensures nothing weird was passed in (i.e. a bug)
	end,
	process_metric_and_advance(Fun,ets:next(?TABLE_NAME,Key)).
	
	
	
