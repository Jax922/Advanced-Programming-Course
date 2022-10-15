-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).


%%% A non-symbolic generator for bst, parameterised by key and value generators
% bst(Key, Value) ->
%     ?LET(KVS, eqc_gen:list({Key, Value}),
%          lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,
%                      empty(),
%                      KVS)).
%% symbolic 
bst(Key, Value) ->
    ?LAZY(
        ?LETSHRINK(KVS, eqc_gen:list({Key, Value}),
            {call, lists, foldl, [
                fun({K,V}, T) -> insert(K, V, T) end,
                {call, bst, empty, []},
                KVS]}
            )
    ).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(eval(T))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, eval(T)))).

%% prop empty/0
prop_empty_valid() ->
    valid (empty()).

%% prop delete/2
prop_delete_valid() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            valid (delete(K, eval(T)))).

%% prop union/2
prop_union_valid() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            valid (union(eval(T1), eval(T2)))).

%%% -- postcondition properties
prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, eval(T))),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, eval(T))
                       end)).

prop_delete_post() ->
    ?FORALL({K1, K2, T},
            {atom_key(), atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, delete(K1, eval(T))),
                        case K1 =:= K2 of
                            true -> nothing;
                            false -> find(K2, eval(T))
                        end)).

prop_union_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, union({branch, leaf, K1, V, leaf}, eval(T))),
                        case K1 =:= K2 of
                            true -> {found, V};
                            false -> find(K2, eval(T))
                        end)).

prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K, insert(K, V, eval(T))),
                       {found, V})).

prop_find_post_absent() -> 
     % ∀ k t. find k (delete k t) === nothing
     ?FORALL({K, _, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
                eqc:equals(find(K, delete(K, eval(T))), nothing)).
                
%%% -- metamorphic properties
%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, eval(T))) >= bst:size(eval(T))).

prop_size_delete() ->
    % ∀ k v t. size (delete k t) <= size t
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            bst:size(delete(K, eval(T))) =< bst:size(eval(T))).

prop_size_union() ->
    % ∀ t1 t2. size (union t1 t2) >= size t1 and size (union t1 t2) >= size t1
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            bst:size(union(eval(T1), eval(T2))) >= bst:size(eval(T1)) 
                    andalso bst:size(union(eval(T1), eval(T2))) >= bst:size(eval(T2))).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(eval(T1)), to_sorted_list(eval(T2))).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, insert(K2, V2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> insert(K2, V2, insert(K1, V1, eval(T)))
                       end)).

prop_insert_delete() ->
    ?FORALL({K1, K2, V1, T},
            {atom_key(), atom_key(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, delete(K2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> delete(K2, insert(K1, V1, eval(T)))
                       end)).

prop_insert_union() ->
    ?FORALL({K, V, T1, T2},
            {atom_key(), int_value(), 
            bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            obs_equals(insert(K, V, union(eval(T1), eval(T2))),
                       union(insert(K, V, eval(T1)), eval(T2)))).

prop_delete_insert() ->
    ?FORALL({K1, K2, V1, T},
            {atom_key(), atom_key(), int_value(),
            bst(atom_key(), int_value())},
            obs_equals(delete(K2, insert(K1, V1, eval(T))),
                       case K1 =:= K2 of
                           true -> 
                                case find(K2, eval(T)) of
                                    nothing -> eval(T);
                                    {found, _} -> delete(K2, eval(T))
                                end;
                           false -> 
                                case find(K2, eval(T)) of
                                    nothing -> insert(K1, V1, eval(T));
                                    {found, _} -> insert(K1, V1, delete(K2, eval(T)))
                                end
                       end)).

prop_delete_delete() ->
    ?FORALL({K1, K2, T},
            {atom_key(), atom_key(), bst(atom_key(), int_value())},
            obs_equals(delete(K1, delete(K2, eval(T))),
                        case K1 =:= K2 of
                            true ->  delete(K1, eval(T));
                            false -> delete(K2, delete(K1, eval(T)))
                        end)).

prop_delete_union() ->
    ?FORALL({K, T1, T2},
            {atom_key(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            obs_equals(delete(K, union(eval(T1), eval(T2))),
                       case find(K, eval(T1)) of
                            nothing -> 
                                case find(K, eval(T2)) of
                                    nothing -> union(eval(T1), eval(T2));
                                    {found, _} -> union(eval(T1), delete(K, eval(T2)))
                                end;
                            {found, _} ->
                                case find(K, eval(T2)) of
                                    nothing -> union(delete(K, eval(T1)), eval(T2));
                                    {found, _} -> union(delete(K, eval(T1)), delete(K, eval(T2)))
                                end
                        end)).

prop_union_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(union({branch, leaf, K1, V1, leaf}, insert(K2, V2, eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eval(T));
                           false -> union({branch, leaf, K2, V2, leaf}, insert(K1, V1, eval(T)))
                       end)).

prop_union_delete() ->
    ?FORALL({K, T1, T2},
            {atom_key(), bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            obs_equals(union(eval(T1), delete(K, eval(T2))),
                        case find(K, eval(T2)) of
                            nothing -> union(eval(T1), eval(T2));
                            {found, _} -> 
                                case find(K, eval(T1)) of
                                    nothing -> delete(K, union(eval(T1), eval(T2)));
                                    {found, _} -> union(eval(T1), eval(T2))
                                end
                        end)).

prop_union_union() ->
    ?FORALL({T1, T2, T3},
            {bst(atom_key(), int_value()), bst(atom_key(), int_value()), 
            bst(atom_key(), int_value())},
            obs_equals(union(eval(T1), union(eval(T2), eval(T3))),
                       union(union(eval(T1), eval(T2)), eval(T3)))).

%%% -- Model based properties
model(T) -> to_sorted_list(T).

prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(model(insert(K, V, eval(T))),
                   sorted_insert(K, V, delete_key(K, model(eval(T)))))).

prop_find_model() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            eqc:equals(find(K, eval(T)),
                    find_key(K, model(eval(T))))).

prop_empty_model() ->
    eqc:equals(model(empty()),[]).

prop_delete_model() ->
    ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            eqc:equals(model(delete(K, eval(T))),
                       delete_key(K, model(eval(T))))).

prop_union_model() ->
    ?FORALL({T1, T2}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
            eqc:equals(model(union(eval(T1), eval(T2))),
                       orddict:merge(fun(_, V1, _) -> V1 end, model(eval(T1)), model(eval(T2))))).

-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].

find_key(_, []) -> nothing;
find_key(Key, [{K, V}| Rest]) ->
    case Key =:= K of
        true -> {found, V};
        false -> find_key(Key, Rest)
    end. 




%% -- Test all properties in the module: eqc:module(test_bst)
