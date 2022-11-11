-module(async).
-behaviour(gen_statem).

-define(NAME, actionid).
-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([init/1]).

init([Fun, Arg]) ->
    State = running,
    Data = #{func => Fun, arg => Arg},
    {ok, State, Data}.

callback_model() -> state_functions.

running(cast, {}) ->

new(Fun, Arg) -> 
    gen_statem:start({local, ?NAME}, ?MODULE, [Fun, Arg], []).

wait(Aid) -> nope.
poll(Aid) -> nope.
wait_catch(Aid) -> nope.
wait_any(Aids) -> nope.
