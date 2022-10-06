-module(test_emoji_copy).
-export([test_all/0]).

% Feel free to use eunit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
        [ test_start_server(), 
          test_stop_server(),
          test_new_shortcode(),
          test_lookup(),
          test_alias(),
          test_delete(),
          test_analytics(),
          test_get_analytics(),
          test_remove_analytics(),
          test_analytics_lookup()] } ].

test_start_server() ->
    [{"We can call start/1 with empty Initial and it returns {ok,E}",
    fun () ->
      Initial = [],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique one value Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique two values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with unique two alias values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"super_smiley", <<240, 159, 152, 131>>}],
      ?assertMatch({ok, _}, emoji:start(Initial))
    end },
    {"We can call start/1 with not unique two values Initial and it returns {ok,E}",
    fun () ->
      Initial = [{"thread", <<240,159,167,181>>}],
      ?assertMatch({error, _}, emoji:start(Initial))
    end }].

test_stop_server() ->
    [{"Starting the server and stopping it",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<100, 100, 100, 100>>}],
      {ok, E} = emoji:start(Initial),
      ?assertEqual(ok, emoji:stop(E))
    end },
    {"Starting the servers and stopping them",
    fun () ->
      Initial1 = [{"smiley", <<240, 159, 152, 131>>}],
      Initial2 = [{"happy", <<240, 159, 152, 131>>}],
      {ok, E1} = emoji:start(Initial1),
      {ok, E2} = emoji:start(Initial2),
      ?assertEqual(ok, emoji:stop(E1)),
      ?assertEqual(ok, emoji:stop(E2))
    end }].  

test_new_shortcode() ->
    [{"Register new shortcode to empty Initial",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "smiley", <<240,159,152,131>>))
    end },
    {"Register new shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "happy", <<222, 222, 222, 222>>))
    end },
    {"Register new shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "funny", <<77, 77, 77, 77>>))
    end },
    {"Register same shortcode to one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<77, 77, 77, 77>>))
    end },
    {"Register same first shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "smiley", <<77, 77, 77, 77>>))
    end },
    {"Register same second shortcode to two unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(S, "happy", <<77, 77, 77, 77>>))
    end },
    {"Register different second shortcode with same emoji",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<222, 222, 222, 222>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(S, "lucky", <<77, 77, 77, 77>>))
    end }].

test_lookup() -> 
    [{"Lookup of shortcode which doesnt exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "smiley"))
    end },
    {"Lookup of shortcode with alias which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<240, 159, 152, 131>>}, emoji:lookup(S, "other_smiley"))
    end },
    {"Lookup of shortcode in two shortcodes which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<222, 222, 222, 222>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual({ok, <<222, 222, 222, 222>>}, emoji:lookup(S, "happy"))
    end },
    {"Lookup of shortcode in two shortcodes which doesnt exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<222, 222, 222, 222>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(no_emoji, emoji:lookup(S, "love"))
    end }].

test_alias() -> 
    [{"Alias of shortcode which doesnt exist",
    fun () ->
      Initial = [],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of shortcode 2 which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "other_smiley"))
    end },
    {"Alias of alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"other_smiley", <<240, 159, 152, 131>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(ok, emoji:alias(S, "other_smiley", "cute_smiley"))
    end },
    {"Alias of alias which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley")),
      ?assertEqual(ok, emoji:alias(S, "other_smiley", "alias_other_smiley"))
    end },
    {"Alias of alias of shortcode which exists",
    fun () ->
      Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
      {ok, S} = emoji:start(Initial),
      ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley")),
      ?assertEqual(ok, emoji:alias(S, "smiley", "other_smiley_alias")),
      ?assertEqual(ok, emoji:alias(S, "smiley", "more_alias"))
    end }].

test_delete() ->
    [{"Delete shortcode from empty Initial",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))     
    end },
    {"Delete shortcode from one unique value Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley"))
    end },
    {"Delete shortcode from one unique value Initial with alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "nice_smiley"),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual(no_emoji, emoji:lookup(S, "nice_smiley"))
    end },
    {"Delete specific shortcode other shortcode no change",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<42, 42, 42, 42>>},
                   {"intelligence", <<99, 99, 99, 99>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:delete(S, "smiley"),
        ?assertEqual(no_emoji, emoji:lookup(S, "smiley")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer")),
        ?assertEqual({ok, <<42, 42, 42, 42>>}, emoji:lookup(S, "happy")),
        ?assertEqual({ok, <<99, 99, 99, 99>>}, emoji:lookup(S, "intelligence"))
    end },
    {"Delete alias and every related shortcode",
    fun () ->
        Initial = [{"happy", <<42, 42, 42, 42>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "happy", "funny"),
        %emoji:alias(S, "happy", "good"),
        emoji:delete(S, "funny"),
        ?assertEqual(no_emoji, emoji:lookup(S, "happy")),
        %?assertEqual(no_emoji, emoji:lookup(S, "good")),
        ?assertEqual(no_emoji, emoji:lookup(S, "funny")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer"))
    end },
    {"Delete specific shortcode other shortcode no change",
    fun () ->
        Initial = [{"happy", <<42, 42, 42, 42>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "happy", "funny"),
        emoji:alias(S, "happy", "good"),
        emoji:alias(S, "software_engineer", "programmer"),
        emoji:alias(S, "software_engineer", "problem_solver"),
        emoji:delete(S, "funny"),
        ?assertEqual(no_emoji, emoji:lookup(S, "happy")),
        ?assertEqual(no_emoji, emoji:lookup(S, "funny")),
        ?assertEqual(no_emoji, emoji:lookup(S, "good")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "software_engineer")),
        %?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "programmer")),
        ?assertEqual({ok, <<100, 100, 100, 100>>}, emoji:lookup(S, "problem_solver"))
    end }].

test_analytics() ->
    [{"Adding analytics to shortcode from empty Initial",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0))     
    end },
    {"Adding analytics to shortcode in Initial",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0))     
    end },
    {"Adding analytics to alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "funny"),
        ?assertEqual(ok, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0))     
    end },
    {"Adding analytics which exists already",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "funny"),
        ?assertEqual(ok, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertMatch({error, _}, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0))   
    end },
    {"Adding different label analytics to two shortcodes",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertEqual(ok, emoji:analytics(S, "happy", fun(_, N) -> N*1 end, "MultiCount", 1))   
    end },
    {"Adding two different analytics to same shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1))   
    end },
    {"Adding two different analytics to shortcode and alias of that shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "funny"),
        ?assertEqual(ok, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1))   
    end },
    {"Adding two same label analytics to shortcode and alias of that shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "funny"),
        ?assertEqual(ok, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertMatch({error, _}, emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "Count", 1))   
    end },
    {"Adding same label analytics to two shortcodes",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        ?assertEqual(ok, emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertEqual(ok, emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Count", 0))   
    end },
    {"Adding same label analytics to two shortcodes and aliases",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>},{"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smile"),
        emoji:alias(S, "happy", "funny"),
        ?assertEqual(ok, emoji:analytics(S, "smile", fun(_, N) -> N+1 end, "Count", 0)),    
        ?assertEqual(ok, emoji:analytics(S, "funny", fun(_, N) -> N+1 end, "Count", 0))   
    end }].

test_get_analytics() ->
    [{"Get the analytics of the not existing shortcode",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the analytics of the existing shortcode empty analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the analytics of the existing shortcode with analytic",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the two analytics of the existing shortcode two analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus",0}, {"Count",0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the three analytics of the existing shortcode three analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        ?assertMatch({ok, [{"MultiCount",1}, {"Minus",0}, {"Count",0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the analytics of the existing shortcode with analytic",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Get the two analytics of the existing shortcode alias two analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smile", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus",0}, {"Count",0}]}, emoji:get_analytics(S, "smile"))     
    end },
    {"Get the three analytics of the existing shortcode alias three analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smile"),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        ?assertMatch({ok, [{"MultiCount",1}, {"Minus",0}, {"Count",0}]}, emoji:get_analytics(S, "smiley_smile"))     
    end },
    {"Get the analytics of the existing shortcode with analytic and not from other shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "happy"))     
    end },
    {"Get the analytics of the existing shortcode alias one analytics and another to different shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Count",0}]}, emoji:get_analytics(S, "smile")),
        ?assertMatch({ok, [{"Minus",0}]}, emoji:get_analytics(S, "happy"))
    end },
    {"Get the three analytics of the existing shortcode alias three analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>},
                   {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smile"),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N*1 end, "MultiCount", 1),
        ?assertMatch({ok, [{"Minus",0}, {"Count",0}]}, emoji:get_analytics(S, "smiley_smile")),
        ?assertMatch({ok, [{"MultiCount",1}]}, emoji:get_analytics(S, "happy")),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "software_engineer"))          
    end }].

test_remove_analytics() ->
    [{"Remove analytics which shortcode doesnt exist",
    fun () ->
        Initial = [],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertMatch({error, _}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which label doesnt exist",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertEqual({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode alias and label exist check by shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:remove_analytics(S, "smiley_smile", "Count"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode alias and label exist check by alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley_smile"))        
    end },
    {"Remove analytics which shortocode alias and label exist check shortcode alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley_smile", fun(_, N) -> N+1 end, "Count", 0),
        emoji:remove_analytics(S, "smiley_smile", "Count"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode and label exist and two analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove analytics which shortocode and label exist and three analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertEqual({ok,[{"MultiCount",1},{"Minus", 0}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove all analytics which shortocode and label exist",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        emoji:remove_analytics(S, "smiley", "Count"),
        emoji:remove_analytics(S, "smiley", "Minus"),
        emoji:remove_analytics(S, "smiley", "MultiCount"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Remove all analytics which shortocode and label with alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        emoji:remove_analytics(S, "smiley_smile", "MultiCount"),
        emoji:remove_analytics(S, "smiley_smile", "Count"),
        emoji:remove_analytics(S, "smiley_smile", "Minus"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley")),     
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley_smile"))
    end },
    {"Remove analytics which shortocode other shortcodes stays same",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<42, 42, 42, 42>>},
                   {"intelligence", <<99, 99, 99, 99>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N+1 end, "Count", 0),
        emoji:remove_analytics(S, "smiley", "Count"),
        ?assertEqual({ok,[{"Minus", 0}]}, emoji:get_analytics(S, "smiley")),     
        ?assertEqual({ok,[{"Count", 0}]}, emoji:get_analytics(S, "happy")),  
        ?assertEqual({ok,[{"Count", 0}]}, emoji:get_analytics(S, "intelligence")),  
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "software_engineer"))  
    end },
    {"Remove all analytics which alias other shortcodes stays same",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<42, 42, 42, 42>>},
                   {"intelligence", <<99, 99, 99, 99>>}, {"software_engineer", <<100, 100, 100, 100>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S, "smiley", "smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "intelligence", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:remove_analytics(S, "smiley_smile", "Count"),
        emoji:remove_analytics(S, "smiley_smile", "Minus"),
        emoji:remove_analytics(S, "smiley_smile", "MultiCount"),
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "smiley")),     
        ?assertEqual({ok,[{"Count", 0}]}, emoji:get_analytics(S, "happy")),  
        ?assertEqual({ok,[{"Minus", 0}, {"Count", 0}]}, emoji:get_analytics(S, "intelligence")),  
        ?assertEqual({ok,[]}, emoji:get_analytics(S, "software_engineer"))      
    end }].

test_analytics_lookup() -> 
    [{"Lookup on emtpy analytics",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, []}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Lookup on one analytics for one shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley"))     
    end },
    {"Lookup on two analytics for one shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley")),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus", 0},{"Count", 1}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Minus", -1},{"Count", 2}]}, emoji:get_analytics(S, "smiley"))
    end },
    {"Lookup on three analytics for one shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        ?assertMatch({ok, [{"MultiCount", 1}, {"Minus", 0},{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"MultiCount", 1}, {"Minus", -1},{"Count", 1}]}, emoji:get_analytics(S, "smiley"))
    end },
    {"Lookup on one analytics for one shortcode other no change",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}, {"happy", <<77, 77, 77, 77>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "happy", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "happy")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley")),     
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "happy"))
    end },
    {"Lookup on two analytics for one shortcode and alias",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:alias(S,"smiley","smiley_smile"),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        ?assertMatch({ok, [{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Count", 1}]}, emoji:get_analytics(S, "smiley")),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        ?assertMatch({ok, [{"Minus", 0},{"Count", 1}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley"),
        ?assertMatch({ok, [{"Minus", -1},{"Count", 2}]}, emoji:get_analytics(S, "smiley")),
        emoji:lookup(S, "smiley_smile"),
        ?assertMatch({ok, [{"Minus", -2},{"Count", 3}]}, emoji:get_analytics(S, "smiley"))        
    end },
    {"Lookup on three analytics for one shortcode",
    fun () ->
        Initial = [{"smiley", <<240, 159, 152, 131>>}],
        {ok, S} = emoji:start(Initial),
        emoji:analytics(S, "smiley", fun(_, N) -> N+1 end, "Count", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N-1 end, "Minus", 0),
        emoji:analytics(S, "smiley", fun(_, N) -> N*1 end, "MultiCount", 1),
        ?assertMatch({ok, [{"MultiCount", 1}, {"Minus", 0},{"Count", 0}]}, emoji:get_analytics(S, "smiley")),
        emoji:alias(S,"smiley","smiley_smile"),
        emoji:lookup(S, "smiley_smile"),
        ?assertMatch({ok, [{"MultiCount", 1}, {"Minus", -1},{"Count", 1}]}, emoji:get_analytics(S, "smiley_smile"))
    end }].