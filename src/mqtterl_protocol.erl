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

-record(state, {
  remaining_bytes = <<>> :: binary(),
  session = undefined
}).

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

handle_packet(?CONNECT, Message#mqtt_connect{client_id = ClientId, clean_session = CleanSession}, Socket, State) ->
  case validate_connect(Socket, Message, [protocol, reserved_flag, client_identifier]) of
    ok ->
      {Session, WasPresent} = case CleanSession of
                                0 ->
                                  mqtterl_sessions:get_or_create(ClientId, #{persistent => true});
                                1 ->
                                  mqtterl_sessions:create(ClientId, #{persistent => false})
                              end,

      Connack = #mqtt_connack{
        session_present = WasPresent,
        return_code = ?CONNACK_ACCEPT
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_connack(Connack)),
      {ok, State};

    _ ->
      {disconnect, State}
  end;



handle_packet(?DISCONNECT, _Message, _Socket, State) ->
  {disconnect, State};

handle_packet(?SUBSCRIBE, #mqtt_subscribe{packet_id = PacketId, topic_filters = TopicFilters}, Socket, State) ->
  Suback = #mqtt_suback{
    packet_id = PacketId,
    return_codes = lists:map(fun(_TopicFilter) ->
      ?QOS0
    end, TopicFilters)
  },
  gen_tcp:send(Socket, mqtterl_codec:encode_suback(Suback)),
  {ok, State};

handle_packet(?PUBLISH, #mqtt_publish{qos = QoS, packet_id = PacketId}, Socket, State) ->
  case QoS of
    ?QOS0 ->
% Expected response: None
      {ok, State};

    ?QOS1 ->
% Expected response: PUBACK
      Puback = #mqtt_puback{
        packet_id = PacketId
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_puback(Puback)),
      {ok, State};

    ?QOS2 ->
% Expected response: PUBREC
% Expected response: PUBACK
      Puback = #mqtt_pubrec{
        packet_id = PacketId
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_pubrec(Puback)),
      {ok, State}
  end;

handle_packet(?PUBREL, #mqtt_pubrel{packet_id = PacketId}, Socket, State) ->
  Pubcomp = #mqtt_pubcomp{
    packet_id = PacketId
  },
  gen_tcp:send(Socket, mqtterl_codec:encode_pubcomp(Pubcomp)),
  {ok, State}.


validate_connect_protocol(Socket, #mqtt_connect{protocol_level = ProtocolLevel, protocol_name = ProtocolName}) ->
  case {ProtocolLevel, ProtocolName} of
    {4, <<"MQTT">>} ->
      true;

    {_, <<"MQTT">>} ->
      Connack = #mqtt_connack{
        session_present = false,
        return_code = ?CONNACK_PROTO_VER
      },
      gen_tcp:send(Socket, mqtterl_codec:encode_connack(Connack)),
      false;

    _ ->
      false

  end.

validate_connect_reserved(Socket, #mqtt_connect{reserved_flag = IsReserved}) ->
  IsReserved == 0.

validate_connect_client_identifier(Socket, #mqtt_connect{client_id = ClientId}) ->
  Size = size(ClientId),
  1 =< Size
    andalso Size =< 23
    andalso case re:run(ClientId, "[0-9a-zA-Z]+") of
              nomatch -> false;
              {match, _} -> true
            end.



