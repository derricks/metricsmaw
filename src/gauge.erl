-module(gauge).

-behaviour(gen_metric).

-export([init/0,add_data/2,type/0,reset/0,purge/1,purge/2,calculate/1]).

type() -> gauge.

init() -> 0.

%% For a gauge, we just care about the value at this time, so just overwrite the state
add_data({Datum,_Timestamp},_State) -> Datum.

reset() -> 0.

purge(State) -> State. % nothing to purge
purge(State,_ExpireSeconds) -> State.

calculate(State) -> State.