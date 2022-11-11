-module(opt_server).
-behaviour(gen_server).

-define(PRINT(Msg), io:format(Msg ++ "\n")).
-define(PRINT(Format, Msg), io:format(Format ++ "\n", Msg)).

-record(account, {id = 0, name="", money=0}).

-export([init/1]).
-export([start/3]).

init([ID, Name, Money]) ->
    ?PRINT("bank account init, ID:~w, Name:~p, Money:~w", [ID, Name, Money]),
    State = #account{
        id = ID,
        name = Name,
        money = Money
    },
    {ok, State}.

start(ID, Name, Money) ->
    gen_server:start_link(?MODULE, [ID, Name, Money], []).

