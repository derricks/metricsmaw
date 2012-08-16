-module(gauge).

-behaviour(gen_metric).

-export([initial_state/1,measure/3,type/0,reset/0,purge/1,calculate/1]).

type() -> gauge.

initial_state(_Config) -> 0.

%% For a gauge, we just care about the value at this time, so just overwrite the state
measure(Data,_ExtraData,_State) -> Data.

reset() -> 0.

purge(State) -> State. % nothing to purge

calculate(State) -> State.