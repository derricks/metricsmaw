-module(counter).
-behaviour(gen_metric).

-export([init/0,add_data/2,type/0,reset/0,purge/1,purge/2,calculate/1]).

type() -> counter.

init() -> 0.

add_data({Datum,_Timestamp},State) -> 
    State + (Datum div 1). % make sure to cast to int it if it's a float

reset() -> 0.

purge(State) -> State. % nothing to purge
purge(State,_ExpireSeconds) -> State.

calculate(State) -> State.
