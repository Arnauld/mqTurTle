%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. avr. 2016 23:13
%%%-------------------------------------------------------------------
-module(dummy_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_illustrate_a_basic_assert__test() ->
  Ref = ok,
  ?assertEqual(ok, Ref).
