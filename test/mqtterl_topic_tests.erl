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
