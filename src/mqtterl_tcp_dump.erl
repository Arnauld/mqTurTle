%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. avr. 2016 09:02
%%%-------------------------------------------------------------------
-module(mqtterl_tcp_dump).
-author("Arnauld").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start(Port) ->
  Ref = mqtterl_tcp_server:start_link(Port, {
    fun() ->
      io:format("handlerFn:Init~n"),
      state
    end,
    fun(_Socket, state, NewData) ->
      <<RLen:16/big-unsigned-integer, RData:RLen/binary>> = NewData,
      io:format("Data received: ~p (size ~p)~n", [RData, RLen]),
      state
    end
  }).