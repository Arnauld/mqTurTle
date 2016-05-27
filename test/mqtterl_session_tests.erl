%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mai 2016 19:56
%%%-------------------------------------------------------------------
-module(mqtterl_session_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_support_subscriptions__usecase_test() ->
  Session0 = mqtterl_session:new({}),
  ?assertEqual(false, mqtterl_session:is_interested_by(Session0, <<"sport/tennis/player1">>)),

  Session1 = mqtterl_session:add_subscriptions(Session0, [<<"sport/#">>]),
  Session2 = mqtterl_session:add_subscriptions(Session1, [<<"info/#">>, <<"#">>]),
  ?assertEqual(true, mqtterl_session:is_interested_by(Session1, <<"sport/tennis/player1">>)),
  ?assertEqual(true, mqtterl_session:is_interested_by(Session2, <<"sport/tennis/player1">>)),

  Session3 = mqtterl_session:remove_subscriptions(Session2, [<<"sport/#">>]),
  ?assertEqual(true, mqtterl_session:is_interested_by(Session3, <<"sport/tennis/player1">>)),

  Session4 = mqtterl_session:remove_subscriptions(Session3, [<<"#">>]),
  ?assertEqual(false, mqtterl_session:is_interested_by(Session4, <<"sport/tennis/player1">>)),
  ?assertEqual(true, mqtterl_session:is_interested_by(Session4, <<"info/tennis/player1">>)).
