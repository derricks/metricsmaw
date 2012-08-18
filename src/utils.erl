-module(utils).

-export([join/2,data_to_string/1,unix_time/0,newline/0,tab/0]).


join(List,Sep) ->
	join(List,[],Sep).
	
	% todo: handle all the integer_to_list cases and so forth
% drained list	
join([],Acc,_Sep) -> Acc;

% when accumulator is new (no sep at beginning)
join([H|T],[] = Acc,Sep) ->     
    join(T,Acc ++ data_to_string(H),Sep);

% every other non-start case
join([H|T],Acc,Sep) ->
	join(T,(Acc ++ Sep) ++ data_to_string(H),Sep).
		
% convert an int or a float to a string
data_to_string(Value) when is_integer(Value) -> integer_to_list(Value);
data_to_string(Value) when is_float(Value) -> float_to_list(Value);
data_to_string(Value) when is_list(Value) -> Value.

% return the number of seconds since the unix epoch
unix_time() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.

newline() -> integer_to_list($\n).

tab() -> integer_to_list($\t).


