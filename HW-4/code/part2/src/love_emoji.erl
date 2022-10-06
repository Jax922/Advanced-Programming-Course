-module(love_emoji).
-export([try_it/0]).

hit(_, N) -> N+1.
accessed(SC, TS) ->
  Now = calendar:local_time(),
  [{SC,Now} | TS].

setup() ->
    {ok, E} = emoji:start([]),
    ok = emoji:new_shortcode(E, "smiley", <<240,159,152,131>>),
    ok = emoji:new_shortcode(E, "poop", <<"\xF0\x9F\x92\xA9">>),
    ok = emoji:alias(E, "poop", "hankey"),
    ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
    ok = emoji:analytics(E, "hankey", fun hit/2, "Counter", 0),
    ok = emoji:analytics(E, "poop", fun accessed/2, "Accessed", []),
    E.

print_analytics(Stats) ->
    lists:foreach(fun({Lab, Res}) -> io:fwrite("  ~s: ~p~n", [Lab, Res]) end,
                  Stats).

try_it() ->
    E = setup(),
    emoji:alias(E, "poop", "poop"),
    State = emoji:get_all(E),
    io:fwrite("~10p~n", [State]), 
    % State = emoji:get_all(E),
    % {ok, Res} = emoji:lookup(E, "hankey"),
    % io:fwrite("~10p~n", [State]),
    % io:fwrite("~s~n", Alias),
    % io:fwrite("~s~n", Analytics),
    
    {ok, Res} = emoji:lookup(E, "hankey"),
    io:fwrite("I looked for :hankey: and got a pile of ~ts~n", [Res]),
   
    {ok, Stats1} = emoji:get_analytics(E, "poop"),
    {ok, Stats2} = emoji:get_analytics(E, "hankey"),
    % io:fwrite("Poppy statistics:~p~n", Stats2), 
    io:fwrite("Poppy statistics:~n"),
    print_analytics(Stats1),
    io:fwrite("Poppy statistics alias:~n"),
    print_analytics(Stats2), 

    % remove analy
    emoji:remove_analytics(E, "poop", "Counter"),
    {ok, Stats3} = emoji:get_analytics(E, "poop"), 
    io:fwrite("Poppy statistics alias:~n"),
    print_analytics(Stats3), 

    io:fwrite("(Hopefully you got a 1 under 'Counter')~n").