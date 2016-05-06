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
  Port = 10123,
  Self = self(),
  try
    Ref = mqtterl_tcp_server:start_link(Port, {
      fun() ->
        io:format("handlerFn:Init~n"),
        state
      end,
      fun(_Socket, state, NewData) ->
        <<RLen:16/big-unsigned-integer, RData:RLen/binary>> = NewData,
        io:format("Data received: ~p (size ~p)~n", [RData, RLen]),
        Self ! RData,
        {disconnect, jobs_done}
      end
    }),

    {ok, S} = gen_tcp:connect({127, 0, 0, 1}, Port, [{packet, 2}]),

    Payload = <<"hello">>,
    Len = size(Payload),
    io:format("Sending ~p (length ~p)~n", [Payload, Len]),
    gen_tcp:send(S, <<Len:16/big-unsigned-integer, Payload/binary>>),

    % block until Data is echoed back :)
    receive
      Data ->
        ?assertEqual(Data, <<Len:16/big-unsigned-integer, Payload/binary>>)
    end
  after
    mqtterl_tcp_server:stop()
  end.