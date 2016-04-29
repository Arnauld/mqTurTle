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

-export([parse_connect_variable_header/1]).

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

parse_connect_variable_header(Bin) ->
  {ProtocolName, Bin1} = parse_utf8(Bin),
  <<ProtocolLevel:8,
  Username:1, Password:1, WillRetain:1, WillQoS:2, WillFlag:1, CleanSession:1, _Reserved:1,
  KeepAlive:16/big-unsigned-integer,
  Bin2/binary>> = Bin1,

  {#mqtt_connect{
    protocol_name = ProtocolName,
    protocol_level = ProtocolLevel,
    username = boolean(Username),
    password = boolean(Password),
    will_retain = boolean(WillRetain),
    will_qos = WillQoS,
    will_flag = boolean(WillFlag),
    clean_session = boolean(CleanSession),
    keep_alive = KeepAlive}, Bin2}.

boolean(1) -> true;
boolean(_) -> false.