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

-include_lib("eunit/include/eunit.hrl").

should_validate_connect_client_identifier__test() ->
  Ref = ok,
  ?assertEqual(ok, Ref).
