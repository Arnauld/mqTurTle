%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. avr. 2016 15:13
%%%-------------------------------------------------------------------
-module(mqtterl_protocol).
-author("Arnauld").
-include("mqtterl_message.hrl").

-record(state, {remaining_bytes = <<>> :: binary()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([tcp_handler/0,
  tcp_handler_init/0,
  tcp_on_packet/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
tcp_handler() ->
  {fun mqtterl_protocol:tcp_handler_init/0, fun mqtterl_protocol:tcp_on_packet/3}.

tcp_handler_init() ->
  #state{}.

tcp_on_packet(Socket, State, NewPacket) ->
  Packet = case State#state.remaining_bytes of
             <<>> ->
               NewPacket;
             Bytes ->
               <<Bytes/binary, NewPacket/binary>>
           end,
  {Type, Message, RemainingBytes} = mqtterl_codec:decode_packet(Packet),
  NewState = handle_packet(Type, Message, Socket, State),
  NewState#state{remaining_bytes = RemainingBytes}.

handle_packet(?CONNECT, _Message, Socket, State) ->
  Connack = #mqtt_connack{
    session_present = false,
    return_code = ?CONNACK_ACCEPT
  },
  gen_tcp:send(Socket, mqtterl_codec:encode_connack(Connack)),
  State;

handle_packet(?DISCONNECT, _Message, _Socket, State) ->
  State;

handle_packet(?SUBSCRIBE, #mqtt_subscribe{packet_id = PacketId, topic_filters = TopicFilters}, Socket, State) ->
  Suback = #mqtt_suback{
    packet_id = PacketId,
    return_codes = lists:map(fun(_TopicFilter) ->
      ?QOS0
    end, TopicFilters)
  },
  gen_tcp:send(Socket, mqtterl_codec:encode_suback(Suback)),
  State;

handle_packet(?PUBLISH, #mqtt_publish{qos = QoS, packet_id = PacketId}, Socket, State) ->
  case QoS of
    ?QOS0 ->
      % Expected response: None
      State;

    ?QOS1 ->
      % Expected response: PUBACK
      Puback = #mqtt_puback{
        packet_id = PacketId
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_puback(Puback)),
      State;

    ?QOS2 ->
      % Expected response: PUBREC
      % Expected response: PUBACK
      Puback = #mqtt_pubrec{
        packet_id = PacketId
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_pubrec(Puback)),
      State
  end;

handle_packet(?PUBREL, #mqtt_pubrel{packet_id = PacketId}, Socket, State) ->
  Pubcomp = #mqtt_pubcomp{
    packet_id = PacketId
  },
  gen_tcp:send(Socket, mqtterl_codec:encode_pubcomp(Pubcomp)),
  State.



