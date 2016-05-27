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

should_support_multi_level_wildcard_on_parent__test() ->
  ?assertEqual(true, mqtterl_topic:match(<<"sport/tennis/player1">>, <<"sport/tennis/player1/#">>)).
