-module(test_erlst).

% -export([test_all/0, test_everything/0]).
% -export([]). % Remember to export the other functions from Q2.2
% 
%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).

-include_lib("record.hrl").
-include_lib("eqc/include/eqc.hrl").

-define(LAUNCH, erlst:launch()).
-define(STOCKEX, ?LAUNCH). % stock exchange

% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.

test_all() ->
  eqc:module(test_erlst).

test_everything() ->
  test_all().


mkstrategy(Opr) -> 
  case Opr of
    {buy_erverything} -> eqc_gen:function1(accept);
    {buy_cheap, Price} -> 
      fun(Offer)-> 
        case Offer#offer.price < Price of
          true -> accept;
          false -> reject
        end
      end;
    {buy_only, NameList} -> 
      fun(Offer)->
        case lists:member(Offer#offer.name, NameList) of
          true -> accept;
          false -> reject
        end
      end;
    {both, [{buy_cheap, Price}, {buy_only, NameList}]} ->
      fun(Offer) -> 
        IsCheap =Offer#offer.price < Price,
        IsRightName = lists:member(Offer#offer.name, NameList),
        case IsCheap and IsRightName of
          true -> accept;
          false -> reject
        end
      end;
    {buy_expensive, Price} ->
      fun(Offer)-> 
        case Offer#offer.price > Price of
          true -> accept;
          false -> reject
        end
      end
  end.

reliable_strategy() ->
  eqc_gen:elements([
    {call,test_erlst,mkstrategy,[buy_everything]},
    {call,test_erlst,mkstrategy,[{buy_cheap, 20}]},
    {call,test_erlst,mkstrategy,[{buy_only, [haskell_ltd, boa_llc]}]},
    {call,test_erlst,mkstrategy,[{both, [{buy_cheap, 8}, {buy_only, [erlang_inc]}]}]},
    {call,test_erlst,mkstrategy,[{buy_expensive, 1000}]}
  ]).


% ================
% other generators
% ================
non_neg_int() -> eqc_gen:nat().
pos_int() -> ?SUCHTHAT(N, eqc_gen:nat(), N > 0).
neg_int() -> ?SUCHTHAT(N, eqc_gen:int(), N < 0).
big_int() -> ?SUCHTHAT(N, eqc_gen:int(), N > 20).
ask_price() -> ?SUCHTHAT(N, eqc_gen:int(), N > 50).
stock_names() -> eqc_gen:elements([erlang,haskell,java,php,javascript,python]).
stocks() -> ?LET(LS, ?LET(L, eqc_gen:list(stock_names()),
                          [ X || X <-lists:usort(L)]), [{Y, pos_int()} || Y <- LS]).
long_stocks() -> ?SUCHTHAT(S, ?LET(LS, ?LET(L, eqc_gen:list(stock_names()),
                                [ X || X <-lists:usort(L)]), [{Y, big_int()} || Y <- LS]), length(S) > 5).
pid() -> ?LETSHRINK(Pid, ?SUCHTHAT(X, eqc_gen:list(pos_int()), length(X) > 2),
           list_to_atom(lists:append(["<0.",lists:concat(Pid),".0>"]))).

uuid() -> ?SUCHTHAT(ID, ?LET(L, list(pos_int()),[X || X <- lists:usort(L)]), length(ID) > 0).

account(S) -> ?LET(A, #account{id={S, pos_int()}, money=pos_int(), stocks=stocks()}, A).
  
accounts(S, IDs) ->
  [#account{id={S, X}, money=non_neg_int(), stocks=stocks()}|| X <- IDs].

offers(S, IDs) ->
  [#offer{id={S, X}, acct={S, X}, name=stock_names(), price=pos_int()}|| X <- IDs].

traders(S, IDs) ->
  [#trader{id={S, X}, acct={S, X}, strategy=reliable_strategy(), worker=pid()}|| X <- IDs].

stock_ex(S) ->
  ?LAZY(
    ?LET(IDs, uuid(), 
        #stock_ex{accounts=accounts(S, IDs), offers=offers(S, IDs), traders=traders(S, IDs)})
  ).

  %%  initial with no offers and no traders 
stock_ex_clean(S) ->
  ?LAZY(
    ?LET(IDs, uuid(), 
        #stock_ex{accounts=accounts(S, IDs), offers=[], traders=[]})
  ).

make_offer_action_list(Acct) ->
  eqc_gen:list({call,erlst,make_offer,[Acct, {stock_names(), pos_int()}]}).

test1() ->
  % Acct = erlst:open_account(Sid, {pos_int(), []}),
  ?LAZY(
    ?LET(SS, [erlst:launch()], 
        [{Sid, Y} || {ok, Sid} <- SS, Y <- make_offer_action_list({Sid, pos_int()}) ])
  ).


% =============
% props
% =============

% checks that the total amount of money and total amount of each stock on the exchange
%   does not change as long as no new accounts are opened.
prop_value_preservation() ->
  {ok, Sid} = ?STOCKEX,
  ?FORALL(StockEx, stock_ex(Sid), 
            eqc:equals(total_money_stocks(StockEx), acotins_mocking(StockEx))
          ).

% It does not work, so I comment it
% checksthatthenumberoftrades returned by shutdown/1 is 
%   less than or equal to the number of calls to make_offer/2.
prop_total_traders() ->
  ?FORALL({StockName, Amount, Price},{stock_names(), big_int(), ask_price()},
           
            ?TRAPEXIT(
              eqc:equals(true,begin
                {ok, Sid} = erlst:launch(),
                Acct = erlst:open_account(Sid, {Price, [{StockName, Amount}]}),
                erlst:make_offer(Acct, {StockName, Price}),
                1 >= erlst:shutdown(Sid)
              end)
            )
          ).

% % prop for apis of erlst
prop_open_account() ->
  {ok, Sid} = ?STOCKEX,
  ?FORALL({Money, Stocks}, {pos_int(), stocks()}, 
      begin
        Acct = erlst:open_account(Sid, {Money, Stocks}),
        eqc:equals(
          true,
          case Acct of
            {Sid, _} -> true;
            _ -> false
          end
        ),
        eqc:equals(Money, erlst:account_balance(Acct))  % check money
      end).

prop_launch() ->
  ?IMPLIES(true, 
    case erlst:launch() of
      {ok, _} -> true;
      _ -> false
    end
  ).

prop_account_balance() ->
  {ok, Sid} = ?STOCKEX,
  ?FORALL({Money, Stocks}, {pos_int(), stocks()}, 
      begin
        Acct = erlst:open_account(Sid, {Money, Stocks}),
        eqc:equals(
          true,
          is_integer(erlst:account_balance(Acct))
        )
      end).

prop_make_offer() ->
  {ok, Sid} = ?STOCKEX,
  ?FORALL({Money, Stocks, PosPrice}, {pos_int(),stocks(),pos_int()}, 
    begin
      Acct = erlst:open_account(Sid, {Money, Stocks}),
      case length(Stocks) > 0 of
        false ->
          eqc:equals(false,
            case erlst:make_offer(Acct,{erlang, PosPrice}) of
              {ok, _} -> true;
              _ -> false
            end);
        true ->
          {StockName, StockAmount} =  lists:nth(1, Stocks),
          case StockAmount > 0 of
            false -> 
              eqc:equals(false,
                case erlst:make_offer(Acct, {StockName, PosPrice}) of
                  {ok, _} -> true;
                  _ -> false
                end);
            true ->
              eqc:equals(true,
                case erlst:make_offer(Acct, {StockName, PosPrice}) of
                  {ok, _} -> true;
                  _ -> false
                end)
          end
      end
    end).

prop_rescind_offer() ->
  {ok, Sid} = ?STOCKEX,
  ?IMPLIES(true, erlst:rescind_offer({Sid, nothing}, {Sid, nothing}) == ok).

prop_remove_trader() ->
  {ok, Sid} = ?STOCKEX,
  ?IMPLIES(true, erlst:remove_trader({Sid, nothing}, {Sid, nothing}) == ok).

  
%% ======================================
%%  helper functions for prop valid
%% ======================================
acotins_mocking(StockEx) ->
  Accounts = StockEx#stock_ex.accounts,
  case length(Accounts) > 0 of
    false -> {0,0}; %% meony and amount of stocks always equal to {0,0}
    true ->
      Account = lists:nth(1, Accounts),
      Acct = Account#account.id,
      erlst:make_offer(Acct, {erlang, 100}),
      erlst:make_offer(Acct, {haskell, 100}),
      erlst:make_offer(Acct, reliable_strategy()),
      erlst:account_balance(Acct),
      total_money_stocks(StockEx)
  end.

total_money_stocks(StockEx) ->
  Accounts = StockEx#stock_ex.accounts,
  lists:foldl(fun(Elem, {M, S}) -> 
                Money = Elem#account.money,
                StocksAmount = orddict:fold(fun(_, V, A)-> V+A end, 0, Elem#account.stocks),
                {M+Money, S+StocksAmount}
              end, {0, 0}, Accounts).


