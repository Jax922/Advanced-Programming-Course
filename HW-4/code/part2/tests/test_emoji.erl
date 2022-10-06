-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

  %  , test_newshotcode()
        %  , test_lookup()
        %  , test_alias()
        %  , test_delete()
        %  , test_stop()]

testsuite() ->
    [ {"======================= start/1 unit test ======================= ", spawn,
       [ test_start_server() ]
      }
      ,{
        "======================= new_shortcode/3 unit test ======================", spawn,
        [test_newshotcode()]
      },{
        "======================= alias/3 unit test ======================", spawn,
        [test_alias()]
      },{
        "======================= delete/2 unit test ======================", spawn,
        [test_delete()]
      },
      {
        "======================= lookup/2 unit test ======================", spawn,
        [test_lookup()]
      },
      {
        "======================= analytics/5 unit test ======================", spawn,
        [test_analytics()]
      },
      {
        "======================= analytics/5 unit test ======================", spawn,
        [test_get_analytics()]
      },
      {
        "======================= analytics/5 unit test ======================", spawn,
        [test_remove_analytics()]
      },
      {
        "======================= analytics/5 unit test ======================", spawn,
        [test_stop()]
      }
    ].

test_start_server() ->
    [{"call start/1 with empty Initial",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end },
    {"call start/1 with medium size Initial",
     fun () ->
       Initial = [{"Leo", <<226,153,140>>}, {"sneezing face", <<240,159,164,167>>}],
       ?assertMatch({ok, _}, emoji:start(Initial))
     end },
    {"call start/1 with small size Initial",
     fun () ->
        Initial = [{"woman_cook", <<"👩‍🍳"/utf8>>}],
       ?assertMatch({ok, _}, emoji:start(Initial))
     end },
    {"error call start/1: same emoji in Initial",
     fun () ->
       Initial = [{"woman_cook", <<"👩‍🍳"/utf8>>},{"woman_cook", <<"👩‍🍳"/utf8>>}],
       ?assertMatch({error, _}, emoji:start(Initial))
     end }].

% test_shortcode_smiley() ->
%     {"Register new shortcode",
%      fun () ->
%        {ok, S} = emoji:start([]),
%        ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
%                                             <<240,159,152,131>>))
%      end }.

test_newshotcode() ->
    [{"call new_shortcode/3 with new shortcode",
    fun () ->
        Initial = [],
        {ok, Eid} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(Eid, "carrot",  <<240,159,165,149>>))
    end },
    {"error call new_shortcode/3 with exist shortcode",
    fun () ->
        Initial = [{"carrot",  <<240,159,165,149>>}],
        {ok, Eid} = emoji:start(Initial),
        ?assertMatch({error, _}, emoji:new_shortcode(Eid, "carrot",  <<240,159,165,149>>))
    end },
    {"call new_shortcode/3 with two same emoji",
    fun () ->
        Initial = [{"thumbsup", <<240,159,145,141>>}],
        {ok, Eid} = emoji:start(Initial),
        ?assertEqual(ok, emoji:new_shortcode(Eid, "+1", <<240,159,145,141>>))
    end }].

test_alias() ->
 [{
    "call alais/3 simple case alias to shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertMatch(ok, emoji:alias(Eid, "thumbsup", "+1"))
    end
  },
  {
    "call alais/3 simple case alias to alias",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      ?assertMatch(ok, emoji:alias(Eid, "+1", "plus one"))
    end
  },
  {"error call alais/3 shortcode  does not exist",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:alias(Eid, "thumbsup", "+1"))
    end },
    {"error call alais/3 alias exist",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      ?assertMatch({error, _}, emoji:alias(Eid, "thumbsup", "+1"))
    end }
   ].

test_delete() ->
    [{"call delete/2 with empty emoji list",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:delete(Eid, "+1"))     
    end },
    {"call delete/2 with exist emoji",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:delete(Eid, "thumbsup")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "thumbsup"))
    end },
    {"call delete/2 with shortcode, it's aliases are gone?",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      ?assertEqual(ok, emoji:delete(Eid, "thumbsup")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "+1")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "plus one"))
    end },
    {"call delete/2 with one alias, shortcode and other aliases are gone?",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      ?assertEqual(ok, emoji:delete(Eid, "+1")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "thumbsup")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "plus one"))
    end },
    {"call delete/2 with alias to alias",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      ?assertEqual(ok, emoji:delete(Eid, "plus one")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "thumbsup")),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "+1"))
    end }].   

test_lookup() ->
    [{"call lookup/2 with non-exist shortcode",
    fun () ->
      {ok, Eid} = emoji:start([]),
      ?assertEqual(no_emoji, emoji:lookup(Eid, "+1"))
    end },
    {"call lookup/2 with exist shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(Eid, "thumbsup"))
    end },
    {"call lookup/2 with alias to shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(Eid, "+1"))
    end },
    {"call lookup/2 with alias to alias",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      ?assertEqual({ok, <<240,159,145,141>>}, emoji:lookup(Eid, "plus one"))
    end }].

test_analytics() ->
   [{"call analytics/5 add a function to a non-exist shortcode",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0))     
    end },
    {"call analytics/5 add a function to a exist shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:analytics(Eid, "thumbsup",fun(_, N) -> N+1 end, "Counter", 0))
    end },
    {"call analytics/5 add two functions with different label",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:analytics(Eid, "thumbsup",fun(_, N) -> N+1 end, "Counter", 0)),
      ?assertEqual(ok, emoji:analytics(Eid, "thumbsup",fun(_, N) -> N+1 end, "Counter1", 0))
    end },
    {"error call analytics/5 add two functions with same label",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:analytics(Eid, "thumbsup",fun(_, N) -> N+1 end, "Counter", 0)),
      ?assertMatch({error, _}, emoji:analytics(Eid, "thumbsup",fun(_, N) -> N+1 end, "Counter", 0))
    end },
    {"call analytics/5 add a functions with alias to shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      ?assertEqual(ok, emoji:analytics(Eid, "+1", fun(_, N) -> N+1 end, "Counter", 0))     
    end },
    {"call analytics/5 add a functions with alias to alias",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      ?assertEqual(ok, emoji:analytics(Eid, "plus one", fun(_, N) -> N+1 end, "Counter", 0))     
    end }
  ].

test_get_analytics() ->
    [{"error call get_alaytics/2 with non-exist shortcode",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertMatch({error, _}, emoji:get_analytics(Eid, "thumbsup"))     
    end },
    {"call get_alaytics/2 with a exist shortcode which is not register function",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertMatch({ok, []}, emoji:get_analytics(Eid, "thumbsup"))     
    end },
    {"call get_alaytics/2 with a exist shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertMatch({ok, [{"Counter", 0}]}, emoji:get_analytics(Eid, "thumbsup"))     
    end },
    {"call get_alaytics/2 with a alias to shortcode",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertMatch({ok, [{"Counter",0}]}, emoji:get_analytics(Eid, "+1"))     
    end },
    {"call get_alaytics/2 with a alias to alias",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:alias(Eid, "thumbsup", "+1"),
      emoji:alias(Eid, "+1", "plus one"),
      emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertMatch({ok, [{"Counter",0}]}, emoji:get_analytics(Eid, "plus one"))      
    end }].

test_remove_analytics() ->
    [{"call remove_analytics/3 a non-exist shortcode",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:remove_analytics(Eid, "thumbsup", "Counter"))     
    end },
    {"call remove_analytics/3 a exist shortcode but does not have functions",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:remove_analytics(Eid, "thumbsup", "Counter"))     
    end },
    {"call remove_analytics/3 a exist shortcode which have functions",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertEqual(ok, emoji:remove_analytics(Eid, "thumbsup", "Counter"))     
    end },
    {"call remove_analytics/3 label is not exist",
    fun () ->
      Initial = [{"thumbsup", <<240,159,145,141>>}],
      {ok, Eid} = emoji:start(Initial),
      emoji:analytics(Eid, "thumbsup", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertEqual(ok, emoji:remove_analytics(Eid, "thumbsup", "Counter1"))       
    end }].

test_stop() ->
    [{"call stop/1 ",
    fun () ->
      Initial = [],
      {ok, Eid} = emoji:start(Initial),
      ?assertEqual(ok, emoji:stop(Eid))
    end }].  