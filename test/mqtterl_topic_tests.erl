%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. mai 2016 09:21
%%%-------------------------------------------------------------------
-module(mqtterl_topic_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_match_direct_topic__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"alert">>, <<"alert">>)).

should_match_direct_topic_even_with_separator__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis">>, <<"sport/tennis">>)).

should_support_multi_level_wildcard_on_empty_parent__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/player1/#">>, <<"sport/tennis/player1">>)).

should_not_support_multi_level_wildcard_on_different_ancestor__test() ->
  ?assertEqual(false, mqtterl_topic:match(<<"sport/tennis/player1/#">>, <<"sport/tennis/player2">>)).

should_support_multi_level_wildcard_with_direct_parent__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/player1/#">>, <<"sport/tennis/player1/ranking">>)).

should_support_multi_level_wildcard_with_multiple_parents__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/player1/#">>, <<"sport/tennis/player1/score/wimbledon">>)).

should_support_multi_level_wildcard_alone__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"#">>, <<"sport/tennis/player1/score/wimbledon">>)).

should_support_single_level_wildcard_on_direct_parent__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/+">>, <<"sport/tennis/player1">>)),
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/+">>, <<"sport/tennis/player2">>)).

should_not_support_single_level_wildcard_on_two_level_parents__test() ->
  ?assertEqual(false, mqtterl_topic:match(<<"sport/tennis/+">>, <<"sport/tennis/player1/ranking">>)).

should_support_single_level_wildcard_on_a_single_level_parent_not_empty_one__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/+">>, <<"sport/">>)),
  ?assertEqual(false, mqtterl_topic:match(<<"sport/+">>, <<"sport">>)).

should_support_single_level_wildcard__non_normative_comments__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"+/+">>, <<"/finance">>)),
  ?assertEqual(true, mqtterl_topic:match(<<"/+">>, <<"/finance">>)),
  ?assertEqual(false, mqtterl_topic:match(<<"+">>, <<"/finance">>)),
  ?assertEqual(true, mqtterl_topic:match(<<"+">>, <<"finance">>)).

should_support_mix_cases__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"+/tennis/#">>, <<"sport/tennis/player1">>)),
  ?assertEqual(true, mqtterl_topic:match(<<"+/tennis/+/score/#">>, <<"sport/tennis/player1/score/wimbledon/set2">>)).
