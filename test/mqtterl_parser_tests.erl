%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 23:17
%%%-------------------------------------------------------------------
-module(mqtterl_parser_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_parse_utf8_binary__test() ->
  Raw1 = <<0, 6, 84, 111, 112, 105, 99, 65>>,
  ?assertEqual({<<"TopicA">>, <<>>}, mqtterl_parser:parse_utf8(Raw1)),
  Raw2 = <<0, 5, 104, 101, 108, 108, 111>>,
  ?assertEqual({<<"hello">>, <<>>}, mqtterl_parser:parse_utf8(Raw2)).

should_encode_utf8_binary__test() ->
  Raw1 = <<0, 6, 84, 111, 112, 105, 99, 65>>,
  ?assertEqual(Raw1, mqtterl_parser:encode_utf8(<<"TopicA">>)),
  Raw2 = <<0, 5, 104, 101, 108, 108, 111>>,
  ?assertEqual(Raw2, mqtterl_parser:encode_utf8(<<"hello">>)).