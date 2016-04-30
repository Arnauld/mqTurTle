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

-record(state, {}).

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
  {Type, Flags, Remaining1} = mqtterl_codec:decode_packet_type(NewPacket),
  handle_packet(Type, Socket, State, Flags, Remaining1).

handle_packet(?CONNECT, Socket, State, _Flags, Remaining1) ->
  {_RemainingLength, Remaining2} = mqtterl_codec:decode_remaining_length(Remaining1),
  {Header, Remaining3} = mqtterl_codec:decode_connect_variable_header(Remaining2),
  {_Payload, _Remaining4} = mqtterl_codec:decode_connect_payload(Header, Remaining3),
  gen_tcp:send(Socket, mqtterl_codec:encode_connack(#mqtt_connack{
    session_present = false,
    return_code = ?CONNACK_ACCEPT
  })),
  State.

