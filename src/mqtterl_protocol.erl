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
  gen_tcp:send(Socket, mqtterl_codec:encode_connack(#mqtt_connack{
    session_present = false,
    return_code = ?CONNACK_ACCEPT
  })),
  State;

handle_packet(?DISCONNECT, _Message, _Socket, State) ->
  State.

