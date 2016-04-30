%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 23:02
%%%-------------------------------------------------------------------
-module(mqtterl_parser).
-author("Arnauld").
-include("mqtterl_message.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([parse_utf8/1, encode_utf8/1]).

%% Message

-export([parse_type/1, parse_remaining_length/1]).

-export([parse_connect_variable_header/1, parse_connect_payload/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
parse_utf8(Bin) ->
  <<Len:16/big, Str:Len/binary, Rest/binary>> = Bin,
  {Str, Rest}.

encode_utf8(Str) ->
  Len = size(Str),
  <<Len:16/big, Str/binary>>.

parse_type(Bin) ->
  <<PacketType:4, Flags:4, Rest/binary>> = Bin,
  {PacketType, Flags, Rest}.

parse_remaining_length(Bin) ->
  parse_remaining_length(Bin, 0, 0).

parse_remaining_length(<<Flag:1, Value:7, Remaining/binary>>, Acc, Level) ->
  NewValue = Acc + Value * remaining_length_multiplier(Level),
  case Flag of
    0 -> {NewValue, Remaining};
    1 -> parse_remaining_length(Remaining, NewValue, Level + 1)
  end.

remaining_length_multiplier(Level) ->
  case Level of
    0 -> 1;
    1 -> 128;
    2 -> 128 * 128;
    3 -> 128 * 128 * 128;
    _ -> error(remining_length_overflow)
  end.

boolean(1) -> true;
boolean(_) -> false.

parse_connect_variable_header(Bin) ->
  {ProtocolName, Bin1} = parse_utf8(Bin),
  <<ProtocolLevel:8,
  Username:1, Password:1, WillRetain:1, WillQoS:2, WillFlag:1, CleanSession:1, _Reserved:1,
  KeepAlive:16/big-unsigned-integer,
  Bin2/binary>> = Bin1,

  {#mqtt_connect{
    protocol_name = ProtocolName,
    protocol_level = ProtocolLevel,
    has_username = boolean(Username),
    has_password = boolean(Password),
    will_retain = boolean(WillRetain),
    will_qos = WillQoS,
    will_flag = boolean(WillFlag),
    clean_session = boolean(CleanSession),
    keep_alive = KeepAlive}, Bin2}.

parse_connect_payload(Header = #mqtt_connect{will_flag = WillFlag, has_username = HasUsername, has_password = HasPassword}, Bin) ->
  {ClientId, Remaining1} = parse_utf8(Bin),
  {WillTopic, Remaining2} = case WillFlag of
                              true -> parse_utf8(Remaining1);
                              false -> {undefined, Remaining1}
                            end,
  {WillMessage, Remaining3} = case WillFlag of
                                true -> parse_utf8(Remaining2);
                                false -> {undefined, Remaining2}
                              end,
  {Username, Remaining4} = case HasUsername of
                             true -> parse_utf8(Remaining3);
                             false -> {undefined, Remaining3}
                           end,
  {Password, Remaining5} = case HasPassword of
                             true -> parse_utf8(Remaining4);
                             false -> {undefined, Remaining4}
                           end,
  {Header#mqtt_connect{
    client_id = ClientId,
    will_topic = WillTopic,
    will_message = WillMessage,
    username = Username,
    password = Password}, Remaining5}.



