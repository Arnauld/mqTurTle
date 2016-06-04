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
-export([handle_packet/4, validate_connect/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
tcp_handler() ->
  {fun mqtterl_protocol:tcp_handler_init/0, fun mqtterl_protocol:tcp_on_packet/3}.

tcp_handler_init() ->
  #state{}.

tcp_on_packet(State, NewPacket, Send) ->
  Packet = case State#state.remaining_bytes of
             <<>> ->
               NewPacket;
             Bytes ->
               <<Bytes/binary, NewPacket/binary>>
           end,
  {Type, Message, RemainingBytes} = mqtterl_codec:decode_packet(Packet),
  error_logger:info_msg("mqtt::packet decoded ~p: ~p", [Type, Message]),

  EncodeAndSend = fun(ResponseType, ResponseMessage) ->
    Encoded = mqtterl_codec:encode_packet(ResponseType, ResponseMessage),
    error_logger:info_msg("mqtt::packet to send ~p: ~p", [ResponseType, ResponseMessage]),
    Send(Encoded)
  end,

  case handle_packet(Type, Message, EncodeAndSend, State) of
    {ok, NewState} ->
      {ok, NewState#state{remaining_bytes = RemainingBytes}};
    Other ->
      Other
  end.

handle_packet(?CONNECT, Message, Send, State) when is_function(Send, 2) ->
  handle_connect(Message, Send, State);

handle_packet(?DISCONNECT, _Message, _Send, State) ->
  {disconnect, normal, State};

handle_packet(?SUBSCRIBE, #mqtt_subscribe{packet_id = PacketId, topic_filters = TopicFilters}, Send, State) when is_function(Send, 2) ->
  Suback = #mqtt_suback{
    packet_id = PacketId,
    return_codes = lists:map(fun(_TopicFilter) ->
      ?QOS0
    end, TopicFilters)
  },
  Send(?SUBACK, Suback),
  {ok, State};

handle_packet(?PUBLISH, #mqtt_publish{qos = QoS, packet_id = PacketId}, Send, State) when is_function(Send, 2) ->
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

handle_packet(?PUBREL, #mqtt_pubrel{packet_id = PacketId}, Send, State) when is_function(Send, 2) ->
  Pubcomp = #mqtt_pubcomp{
    packet_id = PacketId
  },
  Send(?PUBCOMP, Pubcomp),
  {ok, State}.

%% ------------------------------------------------------------------
%% CONNECT
%% ------------------------------------------------------------------

handle_connect(Message, Send, State) ->
  #mqtt_connect{client_id = ClientId, clean_session = CleanSession} = Message,

  Validations = [protocol, reserved_flag, client_identifier],
  case validate_connect(Send, Message, Validations) of
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

      NewState = State#state{session = Session},
      {ok, NewState};

    {invalid, Reason} ->
      {disconnect, Reason, State}
  end.

validate_connect(Send, _Message, []) when is_function(Send, 2) ->
  ok;

validate_connect(Send, Message, [protocol | Rest]) when is_function(Send, 2) ->
  #mqtt_connect{protocol_level = ProtocolLevel, protocol_name = ProtocolName} = Message,
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

validate_connect(Send, Message, [reserved_flag | Rest]) when is_function(Send, 2) ->
  #mqtt_connect{reserved_flag = IsReserved} = Message,
  case IsReserved of
    0 ->
      validate_connect(Send, Message, Rest);
    _ ->
      {invalid, reserved_flag}
  end;

validate_connect(Send, Message, [client_identifier | Rest]) when is_function(Send, 2) ->
  #mqtt_connect{client_id = ClientId} = Message,
  Sz = size(ClientId),
  case size(ClientId) of
    L when 1 =< L andalso L =< 23 ->
      case re:run(ClientId, "[0-9a-zA-Z ]+") of
        {match, [{0, Sz}]} ->
          validate_connect(Send, Message, Rest);


        {match, _} -> % Partial match...
          {invalid, {client_id_chars_partial_match, binary_to_list(ClientId)}};

        nomatch ->
          {invalid, {client_id_chars_no_match, binary_to_list(ClientId)}}
      end;
    _ ->
      {invalid, client_id_size}
  end.



