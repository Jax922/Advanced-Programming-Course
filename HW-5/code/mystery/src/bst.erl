-module(bst).
-export([empty/0, insert/3, delete/2, find/2, union/2]).
-export([valid/1, size/1, to_sorted_list/1, keys/1]).

-opaque bst(Key) :: leaf
                  | {branch, bst(Key), Key, Val :: any(), bst(Key)}.
-export_type([bst/1]).

-spec empty() -> bst(any()).
-spec insert(Key, any(), bst(Key)) -> bst(Key).
-spec delete(Key, bst(Key)) -> bst(Key).
-spec find(Key, bst(Key)) -> nothing | {found, any()}.
-spec union(bst(Key), bst(Key)) -> bst(Key).

% utility
-spec keys(bst(Key)) -> list(Key).
-spec valid (bst(_)) -> boolean().
-spec to_sorted_list(bst(Key)) -> [{Key, any()}].


valid (leaf) -> true;
valid ({branch, L, K, _V, R}) ->
  valid(L) andalso valid(R) andalso
  lists:all(less_than(K), keys(L)) andalso lists:all(greater_than(K), keys(R)).

less_than(K) -> fun(Key) -> Key < K end.
greater_than(K) -> fun(Key) -> K < Key end.

keys(T) -> [ K || {K, _V} <- to_sorted_list(T)].

empty() -> leaf.

find (_K, leaf) -> nothing;
find (K, {branch, L, Key, V, R}) ->
  if K < Key    -> find(K, L);
     K > Key    -> find(K, R);
     K =:= Key  -> {found, V}
  end.

size(T) -> length (keys(T)).

insert (K, V, leaf) -> {branch, leaf, K, V, leaf};
insert (K, V, {branch, L, Key, Val, R}) ->
  if K < Key    -> {branch, insert(K, V, L), Key, Val, R};
     K > Key    -> {branch, L, Key, Val, insert(K, V, R)};
     K =:= Key  -> {branch, L, K, V, R}
  end.


delete (_K, leaf) -> leaf;
delete (K, {branch, L, Key, V, R}) ->
  if K < Key    -> {branch, delete(K, L), Key, V, R};
     K > Key    -> {branch, L, Key, V, delete(K, R)};
     K =:= Key  -> join(L, R)
  end.

-spec join(bst(Key), bst(Key)) -> bst(Key).
join (leaf, R) -> R;
join (L, leaf) -> L;
join ({branch, LL, LK, LV, LR}, {branch, RL, RK, RV, RR}) ->
  {branch, LL, LK, LV, {branch, join(LR, RL), RK, RV, RR}}.


union (leaf, R) -> R;
union (L, leaf) -> L;
union ({branch, L, K, V, R}, T) ->
  {branch, union(L, below(K, T)), K, V, union(R, above(K, T))}.

-spec below(Key, bst(Key)) -> bst(Key).
below (_K, leaf) -> leaf;
below (K, {branch, L, Key, _, _}) when K =< Key -> below(K, L);
below (K, {branch, L, Key, V, R}) -> {branch, L, Key, V, below(K, R)}.

-spec above(Key, bst(Key)) -> bst(Key).
above (_K, leaf) -> leaf;
above (K, {branch, _, Key, _, R}) when K >= Key -> above(K, R);
above (K, {branch, L, Key, V, R}) -> {branch, above(K, L), Key, V, R}.

to_sorted_list (leaf) -> [];
to_sorted_list ({branch, L, K, V, R}) ->
  to_sorted_list(L)++[{K,V} | to_sorted_list(R)].
