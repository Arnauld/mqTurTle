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

% exported mainly for testing purpose
-export([handle_packet/4]).

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
  EncodeAndSend = fun(Type, Message) ->
    Encoded = mqtterl_codec:encode_packet(Type, Message),
    gen_tcp:send(Socket, Encoded)
  end,
  NewState = handle_packet(Type, Message, EncodeAndSend, State),
  NewState#state{remaining_bytes = RemainingBytes}.

handle_packet(?CONNECT, Message, Socket, State) ->
  handle_connect(Message, Socket, State);

handle_packet(?DISCONNECT, _Message, _Socket, State) ->
  {disconnect, State};

handle_packet(?SUBSCRIBE, #mqtt_subscribe{packet_id = PacketId, topic_filters = TopicFilters}, Send, State) ->
  Suback = #mqtt_suback{
    packet_id = PacketId,
    return_codes = lists:map(fun(_TopicFilter) ->
      ?QOS0
    end, TopicFilters)
  },
  Send(?SUBACK, Suback),
  {ok, State};

handle_packet(?PUBLISH, #mqtt_publish{qos = QoS, packet_id = PacketId}, Send, State) ->
  case QoS of
    ?QOS0 ->
% Expected response: None
      {ok, State};

    ?QOS1 ->
% Expected response: PUBACK
      Puback = #mqtt_puback{
        packet_id = PacketId
      },
      Send(?PUBACK, Puback),
      {ok, State};

    ?QOS2 ->
% Expected response: PUBREC
% Expected response: PUBACK
      Pubrec = #mqtt_pubrec{
        packet_id = PacketId
      },
      Send(?PUBREC, Pubrec),
      {ok, State}
  end;

handle_packet(?PUBREL, #mqtt_pubrel{packet_id = PacketId}, Send, State) ->
  Pubcomp = #mqtt_pubcomp{
    packet_id = PacketId
  },
  Send(?PUBCOMP, Pubcomp),
  {ok, State}.

%% ------------------------------------------------------------------
%% CONNECT
%% ------------------------------------------------------------------

handle_connect(Message = #mqtt_connect{client_id = ClientId, clean_session = CleanSession}, Send, State) ->
  case validate_connect(Send, Message, [protocol, reserved_flag, client_identifier]) of
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
      Send(?CONNACK, Connack),
      {ok, State};

    {invalid, Reason} ->
      {disconnect, State}
  end.

validate_connect(_Send, _Message, []) ->
  ok;

validate_connect(Send, Message = #mqtt_connect{protocol_level = ProtocolLevel, protocol_name = ProtocolName}, [protocol | Rest]) ->
  case {ProtocolLevel, ProtocolName} of
    {4, <<"MQTT">>} ->
      validate_connect(Send, Message, Rest);

    {_, <<"MQTT">>} ->
      Connack = #mqtt_connack{
        session_present = false,
        return_code = ?CONNACK_PROTO_VER
      },
      Send(?CONNACK, Connack),
      {invalid, protocol_version};

    _ ->
      {invalid, protocol_name}

  end;

validate_connect(Send, Message = #mqtt_connect{reserved_flag = IsReserved}, [reserved_flag | Rest]) ->
  case IsReserved of
    0 ->
      validate_connect(Send, Message, Rest);
    _ ->
      {invalid, reserved_flag}
  end;

validate_connect(Send, Message = #mqtt_connect{client_id = ClientId}, [client_identifier | Rest]) ->
  case size(ClientId) of
    L when 1 =< L andalso L =< 23 ->
      case re:run(ClientId, "[0-9a-zA-Z]+") of
        nomatch ->
          {invalid, client_id_chars};
        {match, _} ->
          validate_connect(Send, Message, Rest)
      end;
    _ ->
      {invalid, client_id_size}
  end.



