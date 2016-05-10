%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. avr. 2016 23:13
%%%-------------------------------------------------------------------
-module(mqtterl_protocol_tests).
-author("Arnauld").
-include("mqtterl_message.hrl").

-include_lib("eunit/include/eunit.hrl").

should_validate_connect_client_identifier__test() ->
  SendFn = fun(Type, Payload) -> ok end,
  ValidateFn = fun(ClientId) ->
    M = #mqtt_connect{client_id = ClientId},
    mqtterl_protocol:validate_connect(SendFn, M, [client_identifier])
  end,
  %
  ?assertEqual({invalid, client_id_chars}, ValidateFn(<<"aze er">>)),
  ?assertEqual({invalid, client_id_chars}, ValidateFn(<<"aze-er">>)),
  ?assertEqual({invalid, client_id_chars}, ValidateFn(<<"aze_er">>)),
  ?assertEqual({invalid, client_id_chars}, ValidateFn(<<"______">>)),
  ?assertEqual({invalid, client_id_size}, ValidateFn(<<"">>)),
  ?assertEqual({invalid, client_id_size}, ValidateFn(<<"123456789012345678901234">>)),
  ?assertEqual(ok, ValidateFn(<<"012344">>)),
  ?assertEqual(ok, ValidateFn(<<"12345678901234567890123">>)).
