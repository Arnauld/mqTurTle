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

should_start_a_broker_and_accept_connect_packet__test() ->
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