%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. avr. 2016 08:58
%%%-------------------------------------------------------------------
-module(mqtterl_tcp_server_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_connect_to_tcp_server_that_echo_back__test() ->
  Self = self(),
  try
    {ok, ListenSocket, Port} = mqtterl_tcp_server:start_link(0, {
      fun() ->
        io:format("handlerFn:Init~n"),
        state
      end,
      fun(state, NewData, _Send) ->
        <<RLen:16/big-unsigned-integer, RData:RLen/binary>> = NewData,
        io:format("Data received: ~p (size ~p)~n", [RData, RLen]),
        Self ! RData,
        {disconnect, jobs_done}
      end
    }),

    {ok, S} = wait_for_connect(10, Port, none),

    Payload = <<"hello">>,
    Len = size(Payload),
    io:format("Sending ~p (length ~p)~n", [Payload, Len]),
    gen_tcp:send(S, <<Len:16/big-unsigned-integer, Payload/binary>>),

    % block until Data is echoed back :)
    receive
      Data ->
        ?assertEqual(Data, <<Len:16/big-unsigned-integer, Payload/binary>>)
    end
%    gen_tcp:close(S),
%    gen_tcp:close(ListenSocket)
  after
    mqtterl_tcp_server:stop()
  end.

wait_for_connect(0, _, LastError) ->
  LastError;
wait_for_connect(N, Port, _) ->
  case gen_tcp:connect({127, 0, 0, 1}, Port, [{packet, 2}]) of
    {ok, S} ->
      {ok, S};
    {error, Reason} ->
      io:format("Retry connect, got ~p~n", [Reason]),
      timer:sleep(200),
      wait_for_connect(N - 1, Port, {error, Reason})
  end.