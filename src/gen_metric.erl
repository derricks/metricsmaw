%% Behaviour definition for metric processes.
%  Implementers need to export initial_state/1 and add/3
%  initial_state(Config) returns the starting state of the process
%  measure(Data,Extra,State) takes in Data, Extra, and the current State and returns a NewState

-module(gen_metric).

% behaviour callbacks
-export([behaviour_info/1]).

-export([init/2]).

behaviour_info(callbacks) ->
	[
	 %% The initial state for the metric, based on a Config object.
	 {initial_state,1},
	
	 % Given a piece of data, plus extra information, plus the current State
	 % return a new state that takes the current data into account
	 {measure,3},
	
	 % Return the type of the metric (should just return Module)
	 {type,0},
	
	 % Tells the metric to reset itself. Return a new, reset state
	 {reset,0},
	
	 % Tells the metric to purge older data based on the passed-in State. Return a new State to use for future calls
	 {purge,1},
	
	 % Ask the metric for a representation of the current state to pass to reporters. Counters and gauges, for instance,
	 % will just return the passed-in state, while meters might return a compressed view
	 % return value is the calculated form, state is not changed
	 {calculate,1}
	];
	
behaviour_info(_) -> undefined.

init(Module,Config) ->
	loop(Module,Module:initial_state(Config)).
	
loop(Module,State) ->

	receive
		% fire and forget add method. add returns a new state, which becomes the next state
		{add,Data,Extra} ->
			NewState = Module:measure(Data,Extra,State),
			loop(Module,NewState);
			
		% get the current value
		{From,get} ->
			From ! {self(),ok,Module:calculate(State)},
			loop(Module,State);
			
		% get the module type	
		{From,type} ->
			From ! {self(),ok,Module:type()},
			loop(Module,State);
			
		{reset} ->
			NewState = Module:reset(),
			loop(Module,NewState);
			
		{purge} ->
			NewState = Module:purge(State),
			loop(Module,NewState);
			
		{From,details} ->
			From ! {self(),ok,Module:type(),Module:calculate(State)},
			loop(Module,State)
			
    end.
