%%% =================
%%% @doc supervisor for instances of http request handlers
%%% =================
-module(hi_sup).
-behaviour(supervisor).
-define(SERVER,?MODULE).

-export([start_link/2,start_child/1]).

-export([init/1]).

%%% ------------------------
%%% @doc Spin up the supervisor and monitor the specified port on the specified address
%%% ------------------------
start_link(IP,Port) ->
	{ok,Pid} = supervisor:start_link(?MODULE,[IP,Port]),
	start_child(Pid),
	{ok,Pid}.
	
start_child(Server) ->
	supervisor:start_child(Server,[]).
	
init([IP,Port]) ->
	BasicSockOpts = [binary,
	                 {active,false},
	                 {packet,http_bin},
	                 {reuseaddr,true}],
	FinalSockOpts = 
	   case IP of
		   undefined -> BasicSockOpts;
		   _ -> [{ip,IP} | BasicSockOpts]
		end,
	{ok,LSock} = gen_tcp:listen(Port,FinalSockOpts),
	Server = {http_interface,
	          {http_interface,start_link,[LSock]},
	          temporary,
	          brutal_kill,
	          worker,
	          [http_interface]},
	RestartStrategy = {simple_one_for_one,1000,3600},
	{ok,{RestartStrategy,[Server]}}.