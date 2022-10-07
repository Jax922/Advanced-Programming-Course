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
    ok = emoji:alias(E, "hankey", "hankey1"),
    ok = emoji:analytics(E, "smiley", fun(_, N) -> N+1 end, "Counter", 0),
    ok = emoji:analytics(E, "poop", fun hit/2, "Counter", 0),
    ok = emoji:analytics(E, "poop", fun accessed/2, "Accessed", []),
    E.

print_analytics(Stats) ->
    lists:foreach(fun({Lab, Res}) -> io:fwrite("  ~s: ~p~n", [Lab, Res]) end,
                  Stats).

try_it() ->
    E = setup(),
    % emoji:alias(E, "poop", "poop"),

    {ok, Res} = emoji:lookup(E, "hankey"),
    io:fwrite("I looked for :hankey: and got a pile of ~ts~n", [Res]),
   
    % mock test for onlineTA `Register Hit and Last and check that they work with aliases` case
    {ok, Stats1} = emoji:get_analytics(E, "poop"),
    {ok, Stats2} = emoji:get_analytics(E, "hankey"),
    {ok, Stats3} = emoji:get_analytics(E, "hankey1"),
    io:fwrite("Poppy statistics:~n"),
    print_analytics(Stats1),
    io:fwrite("hankey statistics alias:~n"),
    print_analytics(Stats2), 
    io:fwrite("hankey1 statistics alias:~n"),
    print_analytics(Stats3), 

    % remove analy
    emoji:remove_analytics(E, "poop", "Counter"),
    {ok, Stats4} = emoji:get_analytics(E, "poop"), 
    io:fwrite("Poppy statistics alias:~n"),
    print_analytics(Stats4), 

    io:fwrite("(Hopefully you got a 1 under 'Counter')~n").