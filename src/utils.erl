-module(utils).

-export([join/2,data_to_string/1]).


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


