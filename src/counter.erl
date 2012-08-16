-module(counter).
-behaviour(gen_metric).

-export([initial_state/1,measure/3,type/0,reset/0,purge/1,calculate/1]).

type() -> counter.

initial_state(_Config) -> 0.

measure(Data,_Extra,State) -> State + (Data div 1). % make sure to cast to int it if it's a float

reset() -> 0.

purge(State) -> State. % nothing to purge

calculate(State) -> State.
