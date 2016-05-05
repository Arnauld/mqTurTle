%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 23:02
%%%-------------------------------------------------------------------
-module(mqtterl_codec).
-author("Arnauld").
-include("mqtterl_message.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([decode_utf8/1, encode_utf8/1]).

%% Message

-export([decode_packet/1]).
-export([decode_packet_type/1]).
-export([decode_remaining_length/1, encode_remaining_length/1]).

-export([decode_connect_variable_header/1, decode_connect_payload/2]).
-export([encode_connack/1]).
-export([encode_suback/1]).
-export([encode_puback/1]).
-export([encode_pubrec/1]).
-export([encode_pubcomp/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%%
%% see http://erlang.org/doc/programming_examples/bit_syntax.html#Defaults
%%
%% The default unit depends on the the type. For integer, float, and bitstring it is 1. For binary it is 8.
%% The default signedness is unsigned.
%% The default endianness is big.
%% ------------------------------------------------------------------
decode_utf8(<<Len:16, Str:Len/binary, Rest/binary>>) -> {Str, Rest};
decode_utf8(_) -> not_enough_bytes.

encode_utf8(Str) ->
  Len = size(Str),
  <<Len:16, Str/binary>>.

decode_packet_type(<<PacketType:4, Flags:4, Rest/binary>>) -> {PacketType, Flags, Rest};
decode_packet_type(_) -> not_enough_bytes.

-spec(decode_remaining_length(binary()) -> pos_integer() | remaining_length_overflow | not_enough_bytes).
decode_remaining_length(Bin) ->
  decode_remaining_length(Bin, 0, 0).

decode_remaining_length(_, _, 4) ->
  remaining_length_overflow;
decode_remaining_length(<<Flag:1, Value:7, Remaining/binary>>, Acc, Level) ->
  NewValue = Acc + Value * remaining_length_multiplier(Level),
  case Flag of
    0 -> {NewValue, Remaining};
    1 -> decode_remaining_length(Remaining, NewValue, Level + 1)
  end;
decode_remaining_length(_, _, _) ->
  not_enough_bytes.

encode_remaining_length(Num) when Num =< 2#01111111 ->
  <<0:1, Num:7>>;
encode_remaining_length(Num) ->
  Level = Num rem 2#10000000,
  Remaining = Num div 2#10000000,
  Encoded = encode_remaining_length(Remaining),
  <<1:1, Level:7, Encoded/binary>>.

remaining_length_multiplier(0) -> 1;
remaining_length_multiplier(1) -> 128;
remaining_length_multiplier(2) -> 128 * 128;
remaining_length_multiplier(3) -> 128 * 128 * 128.

bit_to_boolean(1) -> true;
bit_to_boolean(_) -> false.
boolean_to_bit(true) -> 1;
boolean_to_bit(_) -> 0.

%% ------------------------------------------------------------------
%% dispatcher
%% ------------------------------------------------------------------

decode_packet(Packet) ->
  case decode_packet_type(Packet) of
    {Type, Flags, Remaining1} ->
      case decode_remaining_length(Remaining1) of
        {RemainingLength, Bin} ->
          case Bin of
          %
          % Only use what is mentioned i.e. RemainingLength
          % Thus no need to check if there is enough bytes: it is or not!
          % fail otherwise if the message is badly formatted
          %
            <<Remaining2:RemainingLength/binary, Remaining3/binary>> ->
              {Type, decode_packet(Type, Flags, Remaining2), Remaining3};

            _ ->
              not_enough_bytes
          end;

        Err ->
          Err
      end;
    Err ->
      Err
  end.

decode_packet(?CONNECT, _Flags, Binaries) ->
  {Header, Remaining} = decode_connect_variable_header(Binaries),
  {Payload, <<>>} = decode_connect_payload(Header, Remaining),
  Payload;

decode_packet(?DISCONNECT, _Flags, <<>>) ->
  #mqtt_disconnect{};

decode_packet(?SUBSCRIBE, _Flags, Binaries) ->
  <<PacketId:16/big-unsigned-integer, Remaining/binary>> = Binaries,
  TopicFilters = decode_subscribe_topic_filters([], Remaining),
  #mqtt_subscribe{
    packet_id = PacketId,
    topic_filters = TopicFilters
  };

decode_packet(?PUBLISH, Flags, Binaries) ->
  <<DUP:1, QoS:2, Retain:1>> = <<Flags:4>>,
  {Topic, Remaining1} = decode_utf8(Binaries),
  {PacketId, Payload} = case QoS of
                          _ when QoS == ?QOS1; QoS == ?QOS2 ->
                            <<PacketId0:16/big-unsigned-integer, RemainingBytes/binary>> = Remaining1,
                            {PacketId0, RemainingBytes};
                          _ ->
                            {undefined, Remaining1}
                        end,
  #mqtt_publish{
    dup = bit_to_boolean(DUP),
    qos = QoS,
    retain = bit_to_boolean(Retain),
    topic = Topic,
    packet_id = PacketId,
    payload = Payload
  };

decode_packet(?PUBREL, Flags, Binaries) ->
  <<0:1, 1:2, 0:1>> = <<Flags:4>>,
  <<PacketId:16/big-unsigned-integer>> = Binaries,
  #mqtt_pubrel{
    packet_id = PacketId
  }.

%% ------------------------------------------------------------------
%% CONNECT
%% ------------------------------------------------------------------
-spec(decode_connect_variable_header(binary()) -> {#mqtt_connect{}, binary()}).
decode_connect_variable_header(Bin) ->
  {ProtocolName, Remaining} = decode_utf8(Bin),
  <<ProtocolLevel:8,
  Username:1, Password:1, WillRetain:1, WillQoS:2, WillFlag:1, CleanSession:1, Reserved:1,
  KeepAlive:16, PayloadBin/binary>> = Remaining,
  {#mqtt_connect{
    protocol_name = ProtocolName,
    protocol_level = ProtocolLevel,
    has_username = bit_to_boolean(Username),
    has_password = bit_to_boolean(Password),
    will_retain = bit_to_boolean(WillRetain),
    will_qos = WillQoS,
    will_flag = bit_to_boolean(WillFlag),
    clean_session = CleanSession,
    keep_alive = KeepAlive,
    reserved_flag = Reserved}, PayloadBin}.

decode_connect_payload(Header = #mqtt_connect{will_flag = WillFlag, has_username = HasUsername, has_password = HasPassword}, Bin) ->
  {ClientId, Remaining1} = decode_utf8(Bin),
  {WillTopic, Remaining2} = case WillFlag of
                              true -> decode_utf8(Remaining1);
                              false -> {undefined, Remaining1}
                            end,
  {WillMessage, Remaining3} = case WillFlag of
                                true -> decode_utf8(Remaining2);
                                false -> {undefined, Remaining2}
                              end,
  {Username, Remaining4} = case HasUsername of
                             true -> decode_utf8(Remaining3);
                             false -> {undefined, Remaining3}
                           end,
  {Password, Remaining5} = case HasPassword of
                             true -> decode_utf8(Remaining4);
                             false -> {undefined, Remaining4}
                           end,
  {Header#mqtt_connect{
    client_id = ClientId,
    will_topic = WillTopic,
    will_message = WillMessage,
    username = Username,
    password = Password}, Remaining5}.


%% ------------------------------------------------------------------
%% CONNACK
%% ------------------------------------------------------------------
encode_connack(#mqtt_connack{return_code = ReturnCode, session_present = SessionPresent}) ->
  SP = boolean_to_bit(SessionPresent),
  <<?CONNACK:4, 0:4,
  2:8,
  0:7, SP:1,
  ReturnCode:8>>.


%% ------------------------------------------------------------------
%% SUBSCRIBE
%% ------------------------------------------------------------------
decode_subscribe_topic_filters(TopicFilters, <<>>) -> lists:reverse(TopicFilters);
decode_subscribe_topic_filters(TopicFilters, Bin) ->
  {Topic, <<_Reserved:6, QoS:2, Remaining/binary>>} = decode_utf8(Bin),
  decode_subscribe_topic_filters([{Topic, QoS} | TopicFilters], Remaining).


%% ------------------------------------------------------------------
%% SUBACK
%% ------------------------------------------------------------------
encode_suback(#mqtt_suback{packet_id = PacketId, return_codes = ReturnCodes}) ->
  Len = 2 + length(ReturnCodes),
  LenBinaries = encode_remaining_length(Len),
  ReturnCodesBinaries = lists:foldl(fun(Code, Acc) ->
    <<Acc/binary, Code:8>>
  end, <<>>, ReturnCodes),
  <<?SUBACK:4, 0:4, LenBinaries/binary, PacketId:16, ReturnCodesBinaries/binary>>.

%% ------------------------------------------------------------------
%% PUBACK
%% ------------------------------------------------------------------
encode_puback(#mqtt_puback{packet_id = PacketId}) ->
  <<?PUBACK:4, 0:4, 2:8, PacketId:16>>.

%% ------------------------------------------------------------------
%% PUBREC
%% ------------------------------------------------------------------
encode_pubrec(#mqtt_pubrec{packet_id = PacketId}) ->
  <<?PUBREC:4, 0:4, 2:8, PacketId:16>>.


%% ------------------------------------------------------------------
%% PUBCOMP
%% ------------------------------------------------------------------
encode_pubcomp(#mqtt_pubcomp{packet_id = PacketId}) ->
  <<?PUBCOMP:4, 0:4, 2:8, PacketId:16>>.

