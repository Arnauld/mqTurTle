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
-include("mqtterl_message.hrl").

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


should_parse_connect_packet__test() ->
  %% `Connects(DUP=False, QoS=0, Retain=False,
  %%           ProtocolName=MQTT, ProtocolVersion=4,
  %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
  %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
  Packet = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,

  {Type, Flags, Remaining} = mqtterl_parser:parse_type(Packet),
  ?assertEqual(?CONNECT, Type).