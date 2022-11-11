-module(erlst).
-behaviour(gen_server).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called erlst.

% Export at least the API:
-export([launch/0,
         shutdown/1,
         open_account/2,
         account_balance/1,
         make_offer/2,
         rescind_offer/2,
         add_trader/2,
         remove_trader/2,
         get_state_test/1
        ]).

% You may have other exports as well
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
        % init/1,
        % handle_call/3,
        % handle_cast/2,
        % handle_info/2,
        % code_change/3,
        % terminate/2

-include("record.hrl").

-define(PRINT(Msg),io:format(Msg ++ "\n")).
-define(PRINT(Format,Msg),io:format(Format ++ "\n",Msg)).
-define(SERVER(Name), {local, Name}).
-define(ACCOUNTID(Sid, Name), {Sid, Name}).
-define(OFFERID(Sid, Offer), {Sid, Offer}).
-define(TRADERID(Sid, Trader), {Sid, Trader}).
init(State) ->
    ?PRINT("stock_ex server starting"),
    {ok, State}.

handle_call(Request, _From, State) ->
    case Request of
        {stop} -> shutdown_helper(State);
        {open_account, Sid, {Money, Stocks}} ->open_account_helper(Sid, {Money, Stocks}, State);
        {account_balance, Acct} ->account_balance_helper(Acct, State);
        {make_offer, Sid, Acct, Offer} ->make_offer_helper(Sid, Acct, Offer, State);
        {add_trader, Acct, Strategy} -> add_trader_helper(_From, Acct, Strategy,State);
        {get_state_test} -> 
            {reply, State, State}
        % {treader_deal, BuyerID, TraderID, Offer} -> treader_deal_helper(BuyerID, TraderID, Offer, State);
    end.

handle_cast(Request, State) ->
    case Request of
        {rescind_offer, OfferID} -> rescind_offer_helper(OfferID, State);
        {remove_trader, Acct, TraderID} -> remove_trader_helper(Acct, TraderID, State)
    end.

% % handle msg from trader worker process
handle_info(Msg, State) ->
    case Msg of 
        {From, fail} ->
            From ! {self(), exec_again},
            {noreply, State};
        {From, deal, Value, TraderID, Acct} ->
            Traders = State#stock_ex.traders,
            case treader_deal_helper(Acct, Value, TraderID, Traders, State) of
                {fail, _, _} -> 
                    From ! {self(), exec_again},
                    {noreply, State};
                {succ,FinalState} -> 
                    From ! {self(), done, From},
                    {noreply, FinalState}
            end;
        {'EXIT', _From, Reason} ->
            io:format("Got exit message: ~p~n", [Reason]),
            {stop, Reason, State}
    end.

terminate(Reason, _) ->
    % case Reason of
    %     {shutdown} ->
    %         ?PRINT("stock exchange shutdown")
    % end,
    ?PRINT("process stop,Reason:~p",[Reason]),
    ok.


%% ==============
%% Client api
%% ==============
-spec launch() -> {ok, stock_exchange()} | {error, term()}.
launch() ->
    {_,_,MS} = erlang:timestamp(),
    Name = list_to_atom("stock_exchane" ++ integer_to_list(MS)),
    State = #stock_ex{accounts = [], offers = [], traders = []},
    gen_server:start_link(?SERVER(Name), ?MODULE, State, []),
    {ok, Name}.

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(Sid) ->
    gen_server:call(Sid,{stop}).

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(Sid, Holding={Money,_}) when Money > 0->
    gen_server:call(Sid, {open_account, Sid, Holding}).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acct={Sid, _}) ->
    gen_server:call(Sid, {account_balance, Acct}).

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(Acct={Sid, _}, Offer) ->
    gen_server:call(Sid, {make_offer, Sid, Acct, Offer}).

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer({Sid,_}, OfferID) ->
    gen_server:cast(Sid, {rescind_offer, OfferID}).

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(Acct={Sid,_}, Strategy) ->
    gen_server:call(Sid, {add_trader, Acct, Strategy}).

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(Acct={Sid,_}, TraderID) ->
    gen_server:cast(Sid, {remove_trader, Acct, TraderID}).

get_state_test({Sid,_}) ->
    gen_server:call(Sid, {get_state_test}).

% helper functions
% 
shutdown_helper(State) ->
    Traders = State#stock_ex.traders,
    DoneTraders = lists:filter(fun(Elem) ->  Elem#trader.status == done end, Traders),
    {stop, shutdown, length(DoneTraders), State}.

open_account_helper(Sid, {Money, Stocks}, State) ->
    Accounts = State#stock_ex.accounts,
    ID = length(Accounts) + 1,
    Acct = ?ACCOUNTID(Sid, ID),
    NewAccount = #account{id= Acct, money = Money, stocks = Stocks},
    NewAccounts = lists:append(Accounts, [NewAccount]),
    NewState = State#stock_ex{accounts=NewAccounts},
    {reply, Acct, NewState}.

account_balance_helper(Acct, State) ->
    Accounts = State#stock_ex.accounts,
    % Search = lists:search(fun(Elem) -> Elem#account.id == Acct end, Accounts),
    case search_one_account(Acct, Accounts) of
        false -> {reply, "This Acct is not exited! please check again!", State};
        {value, Value} -> {reply, Value#account.money, State}
    end.

make_offer_helper(Sid, Acct, {StockName, Price}, State) ->
    case Price < 0 of
        true -> {reply, {error, "Asking price can not less than zero! please check again!"}, State};
        false -> 
            Accounts = State#stock_ex.accounts,
            case search_one_account(Acct, Accounts) of
                false -> {reply, "This Acct is not exited! please check again!", State};
                {value, Value} -> 
                    case get_stock_amount(StockName, Value) of
                        false -> {reply, "You do not have this stock!", State};
                        {value, Amount} ->
                            case Amount >= 1 of
                                false -> {reply, "Amount of this stock less than 1!", State};
                                true -> 
                                    Offers = State#stock_ex.offers,
                                    OfferID = ?OFFERID(Sid, length(Offers) + 1),
                                    NewOffer = #offer{id=OfferID, acct=Acct, name = StockName, price = Price},
                                    NewOffers = lists:append(Offers, [NewOffer]),
                                    NewState = State#stock_ex{offers = NewOffers},
                                    {reply, {ok, OfferID}, NewState}
                            end
                    end
            end
    end.

rescind_offer_helper(OfferID, State) ->
    Offers = State#stock_ex.offers,
    NewOffers = lists:filter(fun(Elem) -> Elem#offer.id /= OfferID end, Offers),
    NewState = State#stock_ex{offers = NewOffers},
    {noreply, NewState}.

remove_trader_helper(_, TraderID, State) ->
    Traders = State#stock_ex.traders,
    %% close corresponding treader worker process
    Value = lists:keyfind(TraderID, 1, Traders),
    case Value of
        false -> null; % can not find this worker
        _ -> 
            WorkerID = Value#trader.worker,
            WorkerID ! {self(), done}
            % exit(whereis(WorkerID), ok)
    end,
    % remove trader and update State
    NewTraders = lists:filter(fun(Elem) -> Elem#trader.id /= TraderID end, Traders),
    NewState = State#stock_ex{traders = NewTraders},
    {noreply, NewState}.


add_trader_helper(_, Acct={Sid, _}, Strategy, State) ->
    Me = self(),
    Traders = State#stock_ex.traders,
    Offers = State#stock_ex.offers,
    TraderID = ?TRADERID(Sid, length(Traders)+1),
    WorkerID = spawn_link(fun()-> 
        Res = lists:search(fun(Elem)-> Strategy(Elem) == accept end, Offers),
        case Res of
            false -> 
                Me ! {self(), fail};
            {value, Value} -> 
                Me ! {self(), deal, Value, TraderID, Acct}
        end,
        receive
            {From, exec_again} ->
                Res = lists:search(fun(Elem)-> Strategy(Elem) == accept end, Offers),
                case Res of
                    false -> From ! {self(), fail};
                    {value, V} -> 
                        From ! {self(), deal, V}
                end;
            {Me, done} -> nothing
                % exit(Wid, "trader worker exit, that is ok")
        end
    end),
    NewTrader = #trader{id=TraderID, acct=Acct, strategy=Strategy, status=installed, worker=WorkerID},
    NewTraders = lists:append(Traders, [NewTrader]),
    NewState = State#stock_ex{traders = NewTraders},
    New = receive
            {WorkerID, fail} -> 
                WorkerID ! {Me, exec_again},
                NewState;
            {WorkerID, deal, Value, _, _} ->
                case treader_deal_helper(Acct, Value, TraderID, NewTrader, NewState) of
                    {fail, _, _} -> 
                        WorkerID ! {Me, exec_again},
                        NewState;
                    {succ,FinalState} -> 
                        WorkerID ! {Me, done},
                        FinalState
                end
        end,
    {reply, TraderID, New}.        

treader_deal_helper(BuyerID, Offer, TraderID, Trader, State) ->
    % There is an offer on the exchange
    Accounts = State#stock_ex.accounts,
    case length(State#stock_ex.offers) > 0 of
        false -> {fail, "There are not any offer on the exchange", State};
        true ->
            % The account that made the offer holds at least 1 unit of the stock being offered.
            Acct = Offer#offer.acct,
            StockName = Offer#offer.name,
            Price = Offer#offer.price,
            case search_one_account(Acct, Accounts) of
                false -> {fail, "can not find this account", State};
                {value, Account} ->
                        case get_stock_amount(StockName, Account) of
                            false -> {fail, "Seller do not have the stock", State};
                            {value, Amount} ->
                                case Amount >= 1 of
                                    false -> {fail, "Amount of the stock less than 1", State};
                                    true ->
                                        % The account that installed the trader has 
                                        % at least the amount of ISK requested by the offer.
                                        case search_one_account(BuyerID, Accounts) of
                                            false -> {fail, "Seller do not have the stock", State};
                                            {value, BuyerAccount} ->
                                                    case BuyerAccount#account.money >= Price of
                                                        false -> {fail, "Not enough moneyy", State};
                                                        true -> 
                                                            % make a deal and update State
                                                            % % update aacount
                                                            SellerMoney = Account#account.money + Price,
                                                            BuyerMoney = BuyerAccount#account.money - Price,
                                                            SellerStock = update_stock_amount(StockName, Account, 1, sub),
                                                            BuyerStock = update_stock_amount(StockName, BuyerAccount, 1, add),
                                                            NewSeller = Account#account{money=SellerMoney, stocks=SellerStock},
                                                            NewBuyer = BuyerAccount#account{money=BuyerMoney, stocks=BuyerStock},
                                                            NewAccounts = update_accounts(NewBuyer, update_accounts(NewSeller, Accounts)),
                                                            % % update trader
                                                            Traders = State#stock_ex.traders,
                                                            OtherTraders = lists:filter(fun(Elem) ->  Elem#trader.id /= TraderID end, Traders),
                                                            NewTrader = Trader#trader{status=done},
                                                            NewTraders = lists:append(OtherTraders, [NewTrader]),
                                                            % % update stock exchange
                                                            NewState = State#stock_ex{accounts=NewAccounts, traders=NewTraders},
                                                            {succ, NewState}
                                                    end
                                        end
                                end
                        end
            end
    end.


%% ========================
%% basic helper funcions
%% ========================

search_one_account(Acct, Accounts) ->
    lists:search(fun(Elem) -> Elem#account.id == Acct end, Accounts).

get_stock_amount(StockName, Account) ->
    Res = orddict:find(StockName, Account#account.stocks),
    case Res of
        error -> false;
        {ok, Value} -> {value, Value}
    end.

update_stock_amount(StockName, Account, Amount, Opt) ->
    orddict:update(StockName, fun(Value) -> 
        case Opt of
            add -> Value + Amount;
            sub -> Value - Amount
        end
    end, Account#account.stocks).

update_accounts(Account, Accounts) ->
    Acct = Account#account.id,
    RestList = lists:filter(fun(Elem) ->  Elem#account.id /= Acct end, Accounts),
    lists:append(RestList, [Account]).