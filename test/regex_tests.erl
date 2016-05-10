%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. mai 2016 22:41
%%%-------------------------------------------------------------------
-module(regex_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_illustrate_regex_usage__test() ->
  {ok, MP} = re:compile("[0-9a-zA-Z]+"),
  Str1 = <<"azerty">>,
  {Res1, Captured} = re:run(Str1, MP),
  ?assertEqual(match, Res1),
  ?assertEqual([{0, size(Str1)}], Captured),

  {Res2, R} = re:run(<<"aze  rty">>, MP),
  io:format("Regex... ~p: ~p: ~n", [Res2, R]),
  ?assertEqual(match, Res2),
  ?assertEqual([{0, 3}], R).
