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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([parse_utf8/1, encode_utf8/1]).

%% Message

-export([parse_type/1]).

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