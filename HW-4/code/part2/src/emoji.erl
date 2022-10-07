-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).
-type initial() :: [{shortcode(), emoji()}].

%% ================================ export functions ===========================%%
-spec start(initial()) -> any().
start(Initial) ->
    case isUnique(Initial) of
        true -> 
            Eid = spawn(fun() -> loop({orddict:from_list(Initial), orddict:from_list([]), []}) end),
            {ok, Eid};
        false -> 
            {error, "Exist same shortcode"}
    end.

-spec new_shortcode(pid(), shortcode(), emoji()) -> any().
new_shortcode(Eid, Short, Emo) -> 
    request_reply(Eid, {new, Short, Emo}).

-spec alias(pid(), shortcode(), shortcode()) -> any().
alias(Eid, Short1, Short2) -> 
    request_reply(Eid, {alias_emo, Short1, Short2}).

-spec delete(pid(), shortcode()) -> any().
delete(Eid, Short) ->
    request_reply(Eid, {del_emo, Short}).

-spec lookup(pid(), shortcode()) -> any().
lookup(Eid, Short) -> 
    request_reply(Eid, {lookup_emo, Short}).

-spec analytics(pid(), shortcode(), analytic_fun(_), string(), _) -> any().
analytics(Eid, Short, Fun, Label, Init) ->
    request_reply(Eid, {analytics_emo, Short, Fun, Label, Init}).

-spec get_analytics(pid(), shortcode()) -> any().
get_analytics(Eid, Short) -> 
    request_reply(Eid, {get_analytics_emo, Short}).

-spec remove_analytics(pid(), shortcode(), string()) -> any().
remove_analytics(Eid, Short, Label) ->
    request_reply(Eid, {remove_analytics_emo, Short, Label}).

-spec stop(pid()) -> any().
stop(Eid) -> 
    process_flag(trap_exit, true),
    try exit(Eid, kill) of
        _ -> ok
    catch
        exit: Reason -> {error, Reason}
    end.

%% ================================ message transit process ===========================%%
request_reply(Eid, Request) ->
    Ref = make_ref(),
    Eid ! {self(), Ref, Request},
    receive
        {_, Response} -> Response
    end.

%% ================================ emoji server ===========================%%
loop(State) -> 
    receive
        {From, Ref, Request} ->
            {NewState, Res} = handle_call(Request, State),
            From ! {Ref,  Res},
            loop(NewState)
    end .

% handle message from client to invoke different helper functions.
handle_call(Request, State) ->
    case Request of
        {new, Short, Emo} -> new_shortcode_helper(State, Short, Emo);
        {alias_emo, Short1, Short2} -> alias_helper(State, Short1, Short2);
        {del_emo, Short} -> delete_helper(State, Short);
        {lookup_emo, Short} -> lookup_helper(State, Short);
        {analytics_emo, Short, Fun, Label, Init} -> analytics_helper(State, Short, Fun, Label, Init);
        {get_analytics_emo, Short} -> get_analytics_helper(State, Short);
        {remove_analytics_emo, Short, Label} -> remove_analytics_helper(State, Short, Label)
    end.

%% ================================ helper functions for handling messages ===========================%%
new_shortcode_helper({EmoList, Alias, Analytics}, Short, Emo) ->
    {NewEmoList, Res}=find_emo(EmoList, Short, Emo),
    {{NewEmoList, Alias, Analytics}, Res}.

alias_helper({EmoList, Alias, Analytics}, Short1, Short2) ->
    NewShort = findShort(EmoList, Alias, Short1),
    if 
        NewShort == Short2 -> % check Short1 is same as Short2
            Res = {error, "can not alias to self"},
            {{EmoList, Alias, Analytics}, Res}; 
        true ->
            IsShortcode = orddict:is_key(Short2, EmoList),
            case IsShortcode of % check Short2 is exist shortcode in EmoList
                false -> 
                    Exist = orddict:is_key(Short2, Alias),
                    case NewShort of % Short1 is not exist
                        error -> 
                            Res = {error, "The short1 does not exist"},
                            {{EmoList, Alias, Analytics}, Res}; 
                        _ ->
                            case Exist of % check Short2 is exist alias in Alias
                                true -> 
                                    Res = {error, "The Short2 alias is exist"},
                                    {{EmoList, Alias, Analytics}, Res};
                                false ->
                                    NewAlias = orddict:store(Short2, NewShort, Alias),
                                    {{EmoList, NewAlias, Analytics}, ok}
                            end
                    end;
                true -> 
                    Res = {error, "can not alias to self"},
                    {{EmoList, Alias, Analytics}, Res}
            end
    end.

delete_helper({EmoList, Alias, Analytics}, Short) ->
    NewShort = findShort(EmoList, Alias, Short),
    NewEmoList = orddict:erase(NewShort, EmoList),
    NewAlias = orddict:filter(fun(_, Val) -> Val /= NewShort end, Alias),
    {{NewEmoList, NewAlias, Analytics}, ok}.

lookup_helper({EmoList, Alias, Analytics}, Short) ->
    NewShort = findShort(EmoList, Alias, Short), 
    case NewShort of
        error ->  {{EmoList, Alias, Analytics}, no_emoji};
        _ -> 
            EmoFind = orddict:find(NewShort, EmoList),
            io:fwrite("EmoFind-------->~p~n", [EmoFind]),
            case EmoFind of 
                {ok, Val} -> 
                    NewAnalytics = execAnalytics(Analytics, NewShort),
                    {{EmoList, Alias, NewAnalytics},{ok, Val}};
                error -> {{EmoList, Alias, Analytics}, no_emoji}
            end
    end.        

analytics_helper({EmoList, Alias, Analytics}, Short, Fun, Label, Init) ->
    NewShort = findShort(EmoList, Alias, Short), 
    SameLabel = isSameLabel(Analytics, NewShort, Label),
    case NewShort of
        error -> {{EmoList, Alias, Analytics}, {error, "con not find this Short"}};
        _ ->
            case SameLabel of
                true -> {{EmoList, Alias, Analytics}, {error, "same label"}};
                false ->
                    Item = #{"short"=>NewShort, "fun"=>Fun, "label"=>Label, "state"=>Init},
                    NewAnalytics = lists:append([Item], Analytics),
                    {{EmoList, Alias, NewAnalytics}, ok}
            end
    end.

get_analytics_helper({EmoList, Alias, Analytics}, Short) ->
    NewShort = findShort(EmoList, Alias, Short),
    case NewShort of
        error ->  {{EmoList, Alias, Analytics}, {error, "con not find this Short"}};
        _ ->
            AnalyList = lists:filter(fun(Elem) -> maps:get("short", Elem) == NewShort end, Analytics),
            case AnalyList of
                [] -> {{EmoList, Alias, Analytics}, {ok, []}};
                _ ->
                    Res = lists:map(fun(Elem) -> 
                        {maps:get("label", Elem), maps:get("state", Elem)}
                        end,
                        AnalyList),
                    {{EmoList, Alias, Analytics}, {ok, Res}}
            end
    end.

remove_analytics_helper({EmoList, Alias, Analytics}, Short, Label) ->
    NewShort = findShort(EmoList, Alias, Short), 
    case NewShort of
        error -> {{EmoList, Alias, Analytics}, ok};
        _ -> 
            NewAnalytics = lists:filter(fun(Elem) -> 
                                        NonShort = maps:get("short", Elem) /= NewShort,
                                        NonLabel = (maps:get("short", Elem) == NewShort) and (maps:get("label", Elem) /= Label),
                                        NonShort or NonLabel
                                        end, Analytics),
            {{EmoList, Alias, NewAnalytics}, ok}
    end.

 %% ================================  basic functions for helper function as above  ===========================%%

 % find the emoji value by a shortcode
find_emo(EmoList, Short, Emo) ->
    Exist = orddict:is_key(Short,EmoList),
    case Exist of
        true -> {EmoList, {error, "This shortcode is exist"}};
        false -> {orddict:store(Short, Emo, EmoList), ok}
    end.

% check shortcodes are unqique in `Initial` 
isUnique([]) -> true;
isUnique([{Short, _} | Tail]) ->
    Keys = [Key || {Key, _} <- Tail, Short == Key],
    case Keys of
        [] -> isUnique(Tail);
        _  -> false
    end.

% check the label is exist when exectue `analytics` function
isSameLabel(Analytics, Short, Label) ->
    AnalyList = lists:filter(fun(Elem) -> maps:get("short", Elem) == Short end, Analytics),
    case AnalyList of
        [] -> false;
        _ -> 
            FindLabel = lists:filter(fun(Elem) -> maps:get("label", Elem) == Label end, AnalyList),
            case FindLabel of
                [] -> false;
                _ -> true
            end
    end. 

% execute analytics functions of one shortcode
execAnalytics(Analytics, Short) ->
    AnalyList = lists:filter(fun(Elem) -> maps:get("short", Elem) == Short end, Analytics), 
    NonAnalyList = lists:filter(fun(Elem) -> maps:get("short", Elem) /= Short end, Analytics),  
    NewAnalyList = lists:map(fun(Elem)-> execOneTime(Elem) end, AnalyList),
    lists:append(NewAnalyList, NonAnalyList).

% helper function for `execAnalytics` function
% exexute the anlaytics one time and update the State of this analytics
execOneTime(Elem) ->
    Fun = maps:get("fun", Elem),
    Short = maps:get("short", Elem),
    State = maps:get("state", Elem),
    NewState = Fun(Short, State),
    maps:update("state", NewState, Elem).    

% input : shortcode or alias
% return : shortcode 
findShort(EmoList, Alias, Short) ->
    Exist = orddict:is_key(Short, EmoList),
    case Exist of
        true -> Short; % if Short is shortcode 
        false ->
            Find = orddict:find(Short, Alias),
            case Find of
                error -> error;
                {ok, Value} -> Value % if Short is alias, then return corresponding shortcode
            end
    end.
