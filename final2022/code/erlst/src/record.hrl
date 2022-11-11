-type stock_exchange() :: atom().
-type account_id() :: {stock_exchange(), non_neg_integer()}.
-type offer_id() :: {stock_exchange(), non_neg_integer()}.
-type trader_id() :: {stock_exchange(), non_neg_integer()}.
-type stock() :: atom().
-type isk() :: non_neg_integer().
-type stock_amount() :: pos_integer().
-type holdings() :: {isk(), [{stock(), stock_amount()}]}.
-type offer() :: {stock(), isk()}.
-type decision() :: accept | reject.
-type trader_strategy() :: fun((offer()) -> decision()).
-type trade_status() :: installed | done.


%%  Data structure define
-record(account, {
    id :: account_id(),
    money :: non_neg_integer(), 
    stocks = [] %  -> [{StockName, Amount}]
}).

-record(offer, {
    id :: offer_id(),
    acct :: account_id(),
    name :: atom(),
    price :: non_neg_integer()
}).
-record(trader, {
    id :: trader_id(),
    acct :: account_id(),
    strategy :: trader_strategy(),
    status:: trade_status(), % 
    worker:: term()
}).
-record(stock_ex, {
    accounts = [],
    offers = [],
    traders = []
}).
