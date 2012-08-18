-module(utils).

-export([join/2,data_to_string/1,unix_time/0,newline/0,tab/0]).


join(List,Sep) ->
	join(List,[],Sep).
	
	% todo: handle all the integer_to_list cases and so forth
% drained list	
join([],Acc,_Sep) -> Acc;

% when accumulator is new (no sep at beginning)
join([H|T],[] = Acc,Sep) when is_integer(H) ->     
    join(T,Acc ++ integer_to_list(H),Sep);

% every other case
join([H|T],Acc,Sep) when is_integer(H) ->
	join(T,(Acc ++ Sep) ++ integer_to_list(H),Sep);
	
% float versions
% when accumulator is new (no sep at beginning)
join([H|T],[] = Acc,Sep) when is_float(H) ->     
    join(T,Acc ++ float_to_list(H),Sep);

% every other case
join([H|T],Acc,Sep) when is_float(H) ->
	join(T,(Acc ++ Sep) ++ float_to_list(H),Sep).
	
% convert an int or a float to a string
data_to_string(Value) when is_integer(Value) -> integer_to_list(Value);
data_to_string(Value) when is_float(Value) -> float_to_list(Value).

% return the number of seconds since the unix epoch
unix_time() -> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.

newline() -> integer_to_list($\n).

tab() -> integer_to_list($\t).


