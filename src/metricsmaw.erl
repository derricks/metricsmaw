-module(metricsmaw).
-behaviour(gen_server).

-define (REFRESH, 60000).

-define(PURGE_RATE, 300000).

-export([start/0,start/1,add_data/4,add_data/3,get/1,socket_client/3,purge_metrics/0]).

% for timer
-export([run_reporters/1]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
%% Server process that can take in metrics and dispatch them to the individual metrics
%% processes. This handles socket events and routes based on metric name

% Tell metric objecs to purge older records
purge_metrics() ->
	lists:foreach(fun({_MetricName,MetricProcess}) -> MetricProcess ! {purge} end,get_metrics()),
	start_metrics_purge_timer().
	
start_metrics_purge_timer() ->
	timer:apply_after(?PURGE_RATE,?MODULE,purge_metrics,[]).

%% start the reporters defined in config and return the list of Pid
start_reporters(List) ->
	
	_Dict = ets:new(reporter_processes,[set,protected,named_table,{read_concurrency,true}]),
	
	% for each configured reporter, spawn a process for it and register
	lists:foldl(
	  fun({ReporterName,ReporterConfig},Acc) -> 
		      Pid = spawn(gen_reporter,process_init,[ReporterName,ReporterConfig]),
		      ets:insert(reporter_processes,{ReporterName,Pid}),
		      [Pid|Acc]
	  end,
	  [],
	  List),
	  timer:apply_after(?REFRESH,?MODULE,run_reporters,[[Config || {_Name,Config} <- List]]).
		
% todo, figure out a better way to do this. foldl seems inefficient (fine for reporters, but maybe not for metrics)
get_reporters() -> 
    ets:match_object(reporter_processes,{'_','_'}).

get_metrics() ->
	ets:match_object(metrics_processes,{'_','_'}).

reporters_foreach(Configs,Fun) ->
	Reporters = get_reporters(),
	ReportersAndConfig = lists:zip(Reporters,Configs),
	EnabledReporters = lists:filter(fun({_Reporter,PropList}) -> proplists:get_value(enabled,PropList,true) end, ReportersAndConfig),
	lists:foreach(Fun,EnabledReporters).
	
	
begin_gather_run(ReporterConfigs) ->
	reporters_foreach(ReporterConfigs,
	    fun({{_ReporterName,ReporterPid},Config}) -> 
		      ReporterPid ! {self(),{gather_start,Config}},
	
		      % make sure each reporter is up and running
		      receive
			       {ok,ready} -> ok
		      end
		   end
	).
	
end_gather_run(Configs) ->
	reporters_foreach(Configs,
	    fun({{_Name,Pid},Config}) -> Pid ! {self(),{gather_end,Config}} end
	).
	

% tell the reporters to get ready and then, for each item in metrics, send it to the reporters	
run_reporters(Configs) ->
	
	begin_gather_run(Configs),
	
	% everyone's now started, so for each metric process, give its data to each reporter
	lists:foreach(
	    fun({MetricName,MetricPid}) ->
		   MetricPid ! {self(),get},
	       MetricData =
		       receive
			        {MetricPid,ok,Data} -> Data
			   end,
			
		   MetricPid ! {self(),type},
		   MetricType =
		       receive
			        {MetricPid,ok,Type} -> Type
			   end,
		      

	        % send the appropriate message to each reporter
	        reporters_foreach(Configs,
	           fun({{_ReporterName,ReporterPid},Config}) -> ReporterPid ! {self(),{gather,atom_to_list(MetricName),MetricType,MetricData,Config}} end
	        )
	
		end,
		get_metrics()
	),

    end_gather_run(Configs),	
	
	% and kick off a timer to do it again
	timer:apply_after(?REFRESH,?MODULE,run_reporters,[Configs]).

%% Socket handling methods
start_socket(Port) ->
	case Port > 0 of
		true ->
			{ok,Listen} = gen_tcp:listen(Port,[binary,{packet,raw},{active,true}]),
			spawn(fun() -> handle_socket_connect(Listen) end);
		    
		false -> {ok,notstarted}
    end.			

handle_socket_connect(Listen) -> 
    {ok,Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> handle_socket_connect(Listen) end),
	handle_socket_data(Socket).
	
handle_socket_data(Socket) ->
	receive
		{tcp,Socket,Bin} -> 
		    Command = socket_message_to_term(Bin),
		    case Command of
			   {add,MetricName,MetricType,Data} ->
				   add_data(MetricName,MetricType,Data),
			       handle_socket_data(Socket);
			   {get,MetricName} ->
				   Current = ?MODULE:get(MetricName),
				   if
					   	 is_float(Current) -> gen_tcp:send(list_to_binary(float_to_list(Current)));
					     is_integer(Current) -> gen_tcp:send(list_to_binary(integer_to_list(Current)));
					     true -> gen_tcp:send(term_to_binary(Current))
				    end,
				   	handle_socket_data(Socket);
			   _ -> % unknown data
				  io:format("Invalid message ~p~n",[Command]),
				  gen_tcp:send(Socket,term_to_binary(Command)),
				  gen_tcp:close(Socket)
			end;
		{tcp_closed,Socket} -> {ok}
	end.
	
% given a chunk of socket data (0 delimited), create the erlang term that will be the command parsed by handle_socket_data	
socket_message_to_term(SocketBin) ->
	binary_to_term(SocketBin).

open_client_socket(Host,Port) ->    {ok,Socket} = gen_tcp:connect(Host,Port,[binary,{packet,raw}]), Socket.

socket_client(Host,Port,{add,_MetricName,_MetricType,_Data}=Command) ->
	Socket = open_client_socket(Host,Port), 
    Bin = term_to_binary(Command),
    ok = gen_tcp:send(Socket,Bin),
    gen_tcp:close(Socket);

socket_client(Host,Port,{get,_MetricName} = Command) ->
	Socket = open_client_socket(Host,Port),
    Bin = term_to_binary(Command),
    ok = gen_tcp:send(Socket,Bin),
    receive 
	    {tcp,Socket,Bin} -> binary_to_term(Bin)
    end,
    gen_tcp:close(Socket).
	

% start the server with a set of options, which are a property list containing
% loads up the data files from a config, puts them under known names, and starts the server loop

% with defaults
start() -> 
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

% with a different config file
start(ConfigFile) -> 
	gen_server:start_link({local,?MODULE},?MODULE,[{config,ConfigFile}],[]).

% the basic call for adding data to the server
% MetricName is any arbitrary (but unique!) name
% MetricType is one of the registred metrics types
% Data is a floating-point number
% Extra is any Erlang term
add_data(MetricName,MetricType,Data) when is_integer(Data);is_float(Data) ->
	add_data(MetricName,MetricType,Data,null).
	
add_data(MetricName,MetricType,Data,Extra) when is_integer(Data); is_float(Data) ->
	gen_server:call(?MODULE,{add,MetricName,MetricType,Data,Extra}).
	

%% get the current value of the given metric
get(MetricName) ->
	gen_server:call(?MODULE,{get,MetricName}).
	
	

%% -------------- gen_server -------------------------

%% Start up the server. Options should be a property list with the following items:
%  config (location of config file, default is metricsmaw.config)
init(Options) -> 
   ConfigFile = proplists:get_value(config,Options,"metricsmaw.config"),
   io:format("Using ~p as a configuration file~n",[ConfigFile]),

   % this reads the whole thing in, basically as a property list
   {ok,Config} = file:consult(ConfigFile),

   io:format("Config: ~p~n",[Config]),

   % create an ets that will map metric name to pid. this allows us to send lists of Pids
   % to reporters (without getting _all_ processes, which may include other things)
   _Dict = ets:new(metrics_processes,[set,protected,named_table,{read_concurrency,true}]),

   ok = timer:start(),

   Reporters = start_reporters(proplists:get_value(reporters,Config,[])),
   io:format("Started reporters ~p~n",[Reporters]),

   start_socket(proplists:get_value(port,Config,18000)),
   start_metrics_purge_timer(),
   
   {ok,Config}.

% todo: what if metric type is invalid
handle_call(
    {add,MetricName,MetricType,Data,Extra},_From,State) ->
		case whereis(MetricName) of
			undefined -> % metric doesn't exist. create
				Pid = spawn(gen_metric,init,[MetricType,State]),
				register(MetricName,Pid),
				ets:insert(metrics_processes,{MetricName,Pid}),
				Pid ! {add,Data,Extra};
			Pid ->
				Pid ! {add,Data,Extra}
		end,
	    {reply,ok,State};	
	
% get request
handle_call(
    {get,MetricName},_From,State) ->
	    Pid = whereis(MetricName),
	    Pid ! {self(),get},
	    receive
		    {Pid, ok, Data} -> {reply,Data,State}
	    end;
	
handle_call(_Request,_From,State) ->
	% todo: this should take in a metric and dispatch it to the appropriate process
	 Reply = ok,	
	{reply,Reply,State}.
	
handle_cast(_Msg,State) ->
	{noreply,State}.
	
handle_info(_Info,State) ->
	{noreply,State}.
	
code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason,_State) ->
	ok.