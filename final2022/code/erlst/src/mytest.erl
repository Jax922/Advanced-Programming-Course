-module(mytest).
-export([testing/0]).

-include("record.hrl").
testing() ->
    io:fwrite("=============testing start=================\n"),
    io:fwrite("=============launch a stock exchange=================\n"),
    {ok, Sid} = erlst:launch(),
    io:fwrite("=============open two accounts=================\n"),
    Acct1 = erlst:open_account(Sid, {1000, [{erlang, 100}, {haskell, 10}]}),
    Acct2 = erlst:open_account(Sid, {2000, [{erlang, 10}, {haskell, 100}]}),
    io:fwrite("=============make offer=================\n"),
    erlst:make_offer(Acct1, {erlang, 10}),
    io:fwrite("=============add trader two times=================\n"),
    erlst:add_trader(Acct2, fun(_) -> accept end),
    erlst:add_trader(Acct2, fun(_) -> reject end),
    State = erlst:get_state_test(Acct1),
    io:fwrite("Now State ~p~n", [State]),
    io:fwrite("=============testing end=================\n").                               
    % treader_deal_helper(Acct2, )
    % erlst:get_state(Acct1).

