%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 23:17
%%%-------------------------------------------------------------------
-module(mqtterl_codec_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").
-include("mqtterl_message.hrl").
-include("assert_ext.hrl").

should_parse_utf8_binary__test() ->
  Raw1 = <<0, 6, 84, 111, 112, 105, 99, 65>>,
  ?assertEqual({<<"TopicA">>, <<>>}, mqtterl_codec:decode_utf8(Raw1)),
  Raw2 = <<0, 5, 104, 101, 108, 108, 111>>,
  ?assertEqual({<<"hello">>, <<>>}, mqtterl_codec:decode_utf8(Raw2)),
  Raw3 = <<0, 0>>,
  ?assertEqual({<<>>, <<>>}, mqtterl_codec:decode_utf8(Raw3)).

should_encode_utf8_binary__test() ->
  Raw1 = <<0, 6, 84, 111, 112, 105, 99, 65>>,
  ?assertEqual(Raw1, mqtterl_codec:encode_utf8(<<"TopicA">>)),
  Raw2 = <<0, 5, 104, 101, 108, 108, 111>>,
  ?assertEqual(Raw2, mqtterl_codec:encode_utf8(<<"hello">>)),
  Raw3 = <<0, 0>>,
  ?assertEqual(Raw3, mqtterl_codec:encode_utf8(<<"">>)).


should_parse_remaining_length__test() ->
  {RL1, _} = mqtterl_codec:decode_remaining_length(<<0:1, 123:7>>),
  ?assertEqual(123, RL1),
  {RL2, _} = mqtterl_codec:decode_remaining_length(<<1:1, 65:7, 0:1, 2:7>>),
  ?assertEqual(321, RL2).

should_encode_remaining_length__test() ->
  RL1 = mqtterl_codec:encode_remaining_length(123),
  ?assertEqual(<<0:1, 123:7>>, RL1),
  RL2 = mqtterl_codec:encode_remaining_length(321),
  ?assertEqual(<<1:1, 65:7, 0:1, 2:7>>, RL2).


should_parse_connect_packet__step_by_step__test() ->
  %% `Connects(DUP=False, QoS=0, Retain=False,
  %%           ProtocolName=MQTT, ProtocolVersion=4,
  %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
  %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
  Packet = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,

  {Type, _Flags, Remaining1} = mqtterl_codec:decode_packet_type(Packet),
  ?assertEqual(?CONNECT, Type),

  {RemainingLength, Remaining2} = mqtterl_codec:decode_remaining_length(Remaining1),
  ?assertEqual(RemainingLength, 22),

  {Header, Remaining3} = mqtterl_codec:decode_connect_variable_header(Remaining2),
  ?assertEqual(<<"MQTT">>, Header#mqtt_connect.protocol_name),
  ?assertEqual(4, Header#mqtt_connect.protocol_level),
  ?assertEqual(false, Header#mqtt_connect.has_username),
  ?assertEqual(false, Header#mqtt_connect.has_password),
  ?assertEqual(0, Header#mqtt_connect.will_qos),
  ?assertEqual(false, Header#mqtt_connect.will_retain),
  ?assertEqual(false, Header#mqtt_connect.will_flag),
  ?assertEqual(1, Header#mqtt_connect.clean_session),
  ?assertEqual(0, Header#mqtt_connect.keep_alive),

  {Payload, Remaining4} = mqtterl_codec:decode_connect_payload(Header, Remaining3),
  ?assertEqual(<<"myclientid">>, Payload#mqtt_connect.client_id),
  ?assertEqual(undefined, Payload#mqtt_connect.username),
  ?assertEqual(undefined, Payload#mqtt_connect.password),
  ?assertEqual(undefined, Payload#mqtt_connect.will_topic),
  ?assertEqual(undefined, Payload#mqtt_connect.will_message),

  % Nothing should remain...
  ?assertEqual(<<>>, Remaining4),
  ok.


should_parse_connect_packet__test() ->
  %% `Connects(DUP=False, QoS=0, Retain=False,
  %%           ProtocolName=MQTT, ProtocolVersion=4,
  %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
  %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
  Packet = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,

  {Type, Message, Remaining} = mqtterl_codec:decode_packet(Packet),

  ?assertEqual(?CONNECT, Type),
  ?assertEqual(<<"MQTT">>, Message#mqtt_connect.protocol_name),
  ?assertEqual(4, Message#mqtt_connect.protocol_level),
  ?assertEqual(false, Message#mqtt_connect.has_username),
  ?assertEqual(false, Message#mqtt_connect.has_password),
  ?assertEqual(0, Message#mqtt_connect.will_qos),
  ?assertEqual(false, Message#mqtt_connect.will_retain),
  ?assertEqual(false, Message#mqtt_connect.will_flag),
  ?assertEqual(1, Message#mqtt_connect.clean_session),
  ?assertEqual(0, Message#mqtt_connect.keep_alive),
  ?assertEqual(<<"myclientid">>, Message#mqtt_connect.client_id),
  ?assertEqual(undefined, Message#mqtt_connect.username),
  ?assertEqual(undefined, Message#mqtt_connect.password),
  ?assertEqual(undefined, Message#mqtt_connect.will_topic),
  ?assertEqual(undefined, Message#mqtt_connect.will_message),

  % Nothing should remain...
  ?assertEqual(<<>>, Remaining),
  ok.

should_detect_not_enough_bytes__connect_packet__test() ->
  %% `Connects(DUP=False, QoS=0, Retain=False,
  %%           ProtocolName=MQTT, ProtocolVersion=4,
  %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
  %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
  Packet = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,
  not_enough_bytes_check(0, Packet).


should_parse_disconnect_packet__test() ->
  %% `Disconnects(DUP=False, QoS=0, Retain=False)`
  Packet = <<224, 0>>,

  {Type, Message, Remaining} = mqtterl_codec:decode_packet(Packet),
  ?assertEqual(?DISCONNECT, Type),
  ?assertEqual(#mqtt_disconnect{}, Message),

  % Nothing should remain...
  ?assertEqual(<<>>, Remaining),
  ok.

should_parse_subscribe_packet__test() ->
  %% `Subscribes(DUP=False, QoS=1, Retain=False, MsgId=2, Data=[('#', 0)])`
  Packet = <<130, 6, 0, 2, 0, 1, 35, 0>>,

  {Type, Message, Remaining} = mqtterl_codec:decode_packet(Packet),
  ?assertEqual(?SUBSCRIBE, Type),
  ?assertEqual(2, Message#mqtt_subscribe.packet_id),
  ?assertEqual([{<<"#">>, 0}], Message#mqtt_subscribe.topic_filters),

  % Nothing should remain...
  ?assertEqual(<<>>, Remaining),
  ok.

should_encode_suback__test() ->
  Suback = #mqtt_suback{packet_id = 17, return_codes = [?QOS0, ?QOS2, ?SUBACK_FAILURE]},
  ?assertEqual(<<?SUBACK:4, 0:4,
  0:1, 5:7, % remaining length 2 (packet id) + 3 (return codes) = 5
  17:16, % packet_id
  ?QOS0:8,
  ?QOS2:8,
  ?SUBACK_FAILURE:8>>, mqtterl_codec:encode_suback(Suback)).


should_parse_publish_packet__test() ->
  %% `Publishes(DUP=False, QoS=0, Retain=False, TopicName='TopicA', Payload=b'qos 0')`
  Packet = <<48, 13, 0, 6, 84, 111, 112, 105, 99, 65, 113, 111, 115, 32, 48>>,

  {Type, Message, Remaining} = mqtterl_codec:decode_packet(Packet),
  ?assertEqual(?PUBLISH, Type),
  ok.


%%
%%
%%

not_enough_bytes_check(Length, Packet) when Length < size(Packet) ->
  io:format("Using ~p on ~p bytes~n", [Length, size(Packet)]),
  BitCount = (Length * 8),
  <<Take:BitCount, _/binary>> = Packet,
  Result = mqtterl_codec:decode_packet(Take),
  ?assertEqualWithMessage(
    io_lib:format("Using ~p on ~p bytes", [Length, size(Packet)]),
    not_enough_bytes,
    Result),
  not_enough_bytes_check(Length + 1, Packet);
not_enough_bytes_check(_, _) -> ok.


