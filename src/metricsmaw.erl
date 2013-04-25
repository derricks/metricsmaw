-module(metricsmaw).
-behaviour(gen_server).

-define (REFRESH, 60000).

-define (REPORTER_LIST, master_list).
-define (REPORTERS_KEY, reporters).

-export([start/0,start/1,get/1,socket_client/3,all_metric_names/0,get_metric_details/1]).

% for timer
-export([run_reporters/0]).

% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,terminate/2]).
%% Server process that can take in metrics and dispatch them to the individual metrics
%% processes. This handles socket events and routes based on metric name

%% start the reporters defined in config and return the list of Pid
start_reporters(List) ->
	
	_Dict = ets:new(reporter_processes,[set,protected,named_table,{read_concurrency,true}]),
	% for each configured reporter, spawn a process for it and register
	lists:foreach(
	  fun({ReporterName,ReporterConfig}) -> 
		      Pid = spawn(gen_reporter,process_init,[ReporterName,ReporterConfig]),
		      ets:insert(reporter_processes,{ReporterName,Pid})
	  end,
	  List),
	 set_reporter_timer().
	
% set a timer to run the reporters
set_reporter_timer() ->
	  Refresh = get_config_key(reporter_refresh,?REFRESH),
	  timer:apply_after(Refresh,?MODULE,run_reporters,[]).
		
% returns the config blob stored in ets
get_config() ->
	[H|_] = ets:lookup(config_data,config),
	H.
	
get_config_key(Key,Default) ->
	{config,Config} = get_config(),
	proplists:get_value(Key,Config,Default).
	
% todo, figure out a better way to do this. foldl seems inefficient (fine for reporters, but maybe not for metrics)
get_reporters() -> 
    ets:match_object(reporter_processes,{'_','_'}).

get_reporter_config(ReporterName) ->
	ReporterConfigs = get_config_key(?REPORTERS_KEY,[]),
	proplists:get_value(ReporterName,ReporterConfigs,[]).

reporters_foreach(Fun) ->
	Reporters = get_reporters(),
	ReportersAndConfigs = lists:map(
	   fun({ReporterName,_Pid}=Record) -> {Record,get_reporter_config(ReporterName)} end,
	   Reporters),
	
	EnabledReporters = lists:filter(fun({_Reporter,PropList}) -> proplists:get_value(enabled,PropList,true) end, ReportersAndConfigs),
	lists:foreach(Fun,EnabledReporters).
	
	
begin_gather_run() ->
	reporters_foreach(
	    fun({{_ReporterName,ReporterPid},Config}) -> 
		      ReporterPid ! {self(),{gather_start,Config}},
	
		      % make sure each reporter is up and running
		      receive
			       {ok,ready} -> ok
		      end
		   end
	).
	
end_gather_run() ->
	reporters_foreach(
	    fun({{_Name,Pid},Config}) -> Pid ! {self(),{gather_end,Config}} end
	).
	

% tell the reporters to get ready and then, for each item in metrics, send it to the reporters	
run_reporters() ->
	
	begin_gather_run(),
	
	% everyone's now started, so for each metric process, give its data to each reporter
	metric_catalog:foreach(
	    fun({MetricName,MetricType},MetricPid)  ->
	        % send the appropriate message to each reporter
	        MetricValue = metric:get_current(MetricPid), % note that here we're in a gen_server call on metric_catalog, so we can't use metric_catalog methods
	                                                     % todo: spawn this in metric_catalog, since there's no result
	        reporters_foreach(
	           fun({{_ReporterName,ReporterPid},Config}) -> ReporterPid ! {self(),{gather,atom_to_list(MetricName),MetricType,MetricValue,Config}} end
	        )
	    end
    ),
    end_gather_run(),	
	
	% and kick off a timer to do it again
	set_reporter_timer().

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
		
		    % record incoming data volumes
		    metrics:add_data('metricsmaw.system.data_rate',meter_minute,byte_size(Bin)),
		    metrics:add_data('metricsmaw.system.socket_requests',meter_minute,1),
		
		    Command = socket_message_to_term(Bin),
		    case Command of
			   {add,MetricName,MetricType,Data} ->
				   metrics:add_data(MetricName,MetricType,Data),
			       handle_socket_data(Socket);
			   {get,MetricName} ->
				   Current = ?MODULE:get(MetricName),
				   gen_tcp:send(Socket,
				      % the result of this gets sent over the socket
				      if
					   	 is_float(Current) -> term_to_binary(Current);
					     is_integer(Current) -> term_to_binary(Current);
					     true -> term_to_binary(Current)
				      end),
				   	handle_socket_data(Socket);
			   {subscribe,MetricName} ->
				   gen_tcp:send(Socket,?MODULE:subscribe(MetricName,Socket)),
				   handle_socket_data(Socket);
				
			   % this handles the generic case where a tuple simply represents a method to be run
			   % specific commands are called out because otherwise you could execute any arbitrary function this way!
			   % note that guard sequences are separated by semicolons, so you have to reproduce the is_tuple call
			   Tuple when is_tuple(Tuple), element(1,Tuple) =:= all_metric_names; 
			              is_tuple(Tuple), element(1,Tuple) =:= get_metric_details ->
				    [FunName|FunArgs] = tuple_to_list(Tuple),
				    gen_tcp:send(Socket,term_to_binary(apply(?MODULE,FunName,FunArgs))),
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

%% get the current value of the given metric
get(MetricName) ->
	gen_server:call(?MODULE,{get,MetricName}).
	
% return the names of all the metrics in the system
all_metric_names() ->
	gen_server:call(?MODULE,{all_metric_names}).

% get full details for a metric: its name, its type, and its current value
get_metric_details(MetricName) ->
	gen_server:call(?MODULE,{get_metric_details,MetricName}).
	
% subscribes the given socket to changes in the given metric
subscribe(_MetricName,_Socket) -> ok.
	
		
	

%% -------------- gen_server -------------------------

%% Start up the server. Options should be a property list with the following items:
%  config (location of config file, default is metricsmaw.config)
init(Options) -> 
   ConfigFile = proplists:get_value(config,Options,"metricsmaw.config"),
   io:format("Using ~p as a configuration file~n",[ConfigFile]),

   % this reads the whole thing in, basically as a property list
   {ok,Config} = file:consult(ConfigFile),

   io:format("Config: ~p~n",[Config]),

   % create an ets table for holding the config object
   _ConfigDict = ets:new(config_data,[set,protected,named_table,{read_concurrency,true}]),
   ets:insert(config_data,{config,Config}),

   %% todo: move as part of application definition
   metric_sup:start_link(),
   metric_catalog:start_link(),
   metric_custodian:start_link(),
   hi_sup:start_link(undefined,8000),

   ok = timer:start(),

   Reporters = start_reporters(proplists:get_value(reporters,Config,[])),
   io:format("Started reporters ~p~n",[Reporters]),

   start_socket(proplists:get_value(port,Config,18000)),
   
   {ok,Config}.

handle_call(_Request,_From,State) ->
	{reply,undefined,State}.
	
handle_cast(_Msg,State) ->
	{noreply,State}.
	
handle_info(_Info,State) ->
	{noreply,State}.
	
code_change(_OldVsn, N, _Extra) -> {ok, N}.

terminate(_Reason,_State) ->
	ok.