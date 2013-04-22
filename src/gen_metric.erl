%%% Define template for a single metric 
-module(gen_metric).

% behaviour callbacks
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	[
	 %% The initial state for the metric, based on a Config object.
	 {init,0},
	
	 % Given a piece of data, plus extra information, plus the current State
	 % return a new state that takes the current data into account
	 {add_data,2},
	
	 % Return the type of the metric (should just return Module)
	 {type,0},
	
	 % Tells the metric to reset itself. Return a new, reset state
	 {reset,0},
	
	 % Tells the metric to purge older data based on the passed-in State. Return a new State to use for future calls
	 {purge,1},
	
	 % Tells the metric to purge data from state that's older than the specified number of seconds
 	 {purge,2},
	
	 % Ask the metric for a representation of the current state to pass to reporters. Counters and gauges, for instance,
	 % will just return the passed-in state, while meters might return a compressed view
	 % return value is the calculated form, state is not changed
	 {calculate,1}
	];
	
behaviour_info(_) -> undefined.