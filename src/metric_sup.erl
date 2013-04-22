%%% =========================
%%% @doc Supervisor for metric processes. It's set up with simple_one_for_one so that metrics can be killed and spawned easily
%%% ========================
-module(metric_sup).

-behaviour(supervisor).

-define(SERVER,?MODULE).

-export([init/1]).

-export([start_link/0,start_child/1]).

start_link() ->
	supervisor:start_link({local,?SERVER},?MODULE,[]).
	
start_child(MetricType) ->
	% note that arguments passed to start_child are appended to the fixed list defined in init's Children list
	supervisor:start_child(?SERVER,[MetricType]).
	
init([]) ->
	Element = {metric,{metric,start_link,[]},temporary,brutal_kill,worker,[metric]},
	Children = [Element],
	RestartStrategy = {simple_one_for_one,0,1},
	{ok,{RestartStrategy,Children}}.