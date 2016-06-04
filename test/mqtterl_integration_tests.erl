%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. mai 2016 09:16
%%%-------------------------------------------------------------------
-module(mqtterl_integration_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_start_a_broker_and_support_connect_packet__test() ->
  mqtterl_sessions:start_link(),
  try
    {ok, ListenSocket, Port} = mqtterl_tcp_server:start_link(0, mqtterl_protocol:tcp_handler()),
    try
      {ok, Client} = wait_for_connect(10, Port, none),

      %% Simple scenario: connect then disconnect
      %% `Connects(DUP=False, QoS=0, Retain=False,
      %%           ProtocolName=MQTT, ProtocolVersion=4,
      %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
      %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
      Connect = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,
      gen_tcp:send(Client, Connect),

      %% {active, false} is required to retrieve bytes from `gen_tcp:recv`
      {ok, ConnectAck} = gen_tcp:recv(Client, 0),

      ?assertEqual(<<2:4, 0:4, % fixed header
      2:8, % remaining length
      0:7, 0:1, % Session present flag
      0:8 % connect return code: accepted
      >>, list_to_binary(ConnectAck))
    after
      gen_tcp:shutdown(ListenSocket, read_write),
      mqtterl_tcp_server:stop()
    end
  after
    mqtterl_sessions:stop()
  end.

should_start_a_broker_and_support_connect_and_disconnect_roundtrip_packet__test() ->
  mqtterl_sessions:start_link(),
  try
    {ok, ListenSocket, Port} = mqtterl_tcp_server:start_link(0, mqtterl_protocol:tcp_handler()),
    try
      {ok, Client} = wait_for_connect(10, Port, none),

      %% Simple scenario: connect then disconnect
      %% `Connects(DUP=False, QoS=0, Retain=False,
      %%           ProtocolName=MQTT, ProtocolVersion=4,
      %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
      %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
      Connect = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,
      gen_tcp:send(Client, Connect),

      {ok, ConnectAck} = gen_tcp:recv(Client, 0),
      ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, 0:8>>, list_to_binary(ConnectAck)),

      Disconnect = <<224, 0>>,
      gen_tcp:send(Client, Disconnect)


    after
      gen_tcp:shutdown(ListenSocket, read_write),
      mqtterl_tcp_server:stop()
    end
  after
    mqtterl_sessions:stop()
  end.


should_start_a_broker_and_support_subscribe_and_publish_usecase__test() ->
  mqtterl_sessions:start_link(),
  try
    {ok, ListenSocket, Port} = mqtterl_tcp_server:start_link(0, mqtterl_protocol:tcp_handler()),
    try
      {ok, Client1} = wait_for_connect(10, Port, none),
      {ok, Client2} = wait_for_connect(10, Port, none),

      %% Simple scenario: connect then disconnect
      %% `Connects(DUP=False, QoS=0, Retain=False,
      %%           ProtocolName=MQTT, ProtocolVersion=4,
      %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
      %%           ClientId=myclientid, usernameFlag=False, passwordFlag=False)`
      Connect1 = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 109, 121, 99, 108, 105, 101, 110, 116, 105, 100>>,
      gen_tcp:send(Client1, Connect1),
      {ok, ConnectAck1} = gen_tcp:recv(Client1, 0),
      ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, 0:8>>, list_to_binary(ConnectAck1)),

      %% `Subscribes(DUP=False, QoS=1, Retain=False, MsgId=2, Data=[('#', 0)])`
      Subscribe = <<130, 6, 0, 2, 0, 1, 35, 0>>,
      gen_tcp:send(Client1, Subscribe),

      {ok, SubAck} = gen_tcp:recv(Client1, 0),
      ?assertEqual(<<9:4, 0:4, 3, 0, 2, 0>>, list_to_binary(SubAck)),

      %% Simple scenario: connect then disconnect
      %% `Connects(DUP=False, QoS=0, Retain=False,
      %%           ProtocolName=MQTT, ProtocolVersion=4,
      %%           CleanSession=True, WillFlag=False, KeepAliveTimer=0,
      %%           ClientId=otclientid, usernameFlag=False, passwordFlag=False)`
      Connect2 = <<16, 22, 0, 4, 77, 81, 84, 84, 4, 2, 0, 0, 0, 10, 111, 116, 99, 108, 105, 101, 110, 116, 105, 100>>,
      gen_tcp:send(Client2, Connect2),
      {ok, ConnectAck2} = gen_tcp:recv(Client2, 0),
      ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, 0:8>>, list_to_binary(ConnectAck2)),

      %% `Publishes(DUP=False, QoS=1, Retain=False, TopicName='TopicA', Payload=b'qos 0')`
      Publish = <<3:4, 0:1, 1:2, 0:1, %
      13, 0,
      6, 84, 111, 112, 105, 99, 65, 113, 111, 115, 32, 48>>,
      error_logger:info_msg("Client2: Publishing message"),
      gen_tcp:send(Client2, Publish),

      error_logger:info_msg("Client2: Waiting for puback"),
      {ok, PubAck} = gen_tcp:recv(Client2, 0),
      ?assertEqual(<<2:4, 0:4, 2:8, 0:7, 0:1, 0:8>>, list_to_binary(PubAck)),

      error_logger:info_msg("Client1: Waiting for published message"),
      {ok, Received} = gen_tcp:recv(Client1, 0),
      ?assertEqual(Publish, list_to_binary(Received)),


      Disconnect = <<224, 0>>,
      gen_tcp:send(Client1, Disconnect),
      gen_tcp:send(Client2, Disconnect)


    after
      gen_tcp:shutdown(ListenSocket, read_write),
      mqtterl_tcp_server:stop()
    end
  after
    mqtterl_sessions:stop()
  end.



wait_for_connect(0, _, LastError) ->
  LastError;
wait_for_connect(N, Port, _) ->
  case gen_tcp:connect({127, 0, 0, 1}, Port, [{packet, 0}, {active, false}]) of
    {ok, S} ->
      {ok, S};
    {error, Reason} ->
      io:format("Retry connect, got ~p~n", [Reason]),
      timer:sleep(200),
      wait_for_connect(N - 1, Port, {error, Reason})
  end.