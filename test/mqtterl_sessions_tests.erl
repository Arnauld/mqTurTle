%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. mai 2016 21:38
%%%-------------------------------------------------------------------
-module(mqtterl_sessions_tests).
-author("Arnauld").

-include_lib("eunit/include/eunit.hrl").

should_create_a_new_session_when_does_not_yet_exists__test() ->
  try
    mqtterl_sessions:start_link(),
    {Session, WasPresent} = mqtterl_sessions:get_or_create(<<"cli1">>, #{}),
    ?assertEqual(false, WasPresent)
  after
    mqtterl_sessions:stop()
  end.

should_return_existing_session_when_it_already_exists__test() ->
  try
    mqtterl_sessions:start_link(),
    {Session1, WasPresent1} = mqtterl_sessions:get_or_create(<<"cli1">>, #{}),
    {Session2, WasPresent2} = mqtterl_sessions:get_or_create(<<"cli1">>, #{}),
    ?assertEqual(false, WasPresent1),
    ?assertEqual(true, WasPresent2),
    ?assertEqual(Session1, Session2)
  after
    mqtterl_sessions:stop()
  end.

should_create_a_new_session__test() ->
  try
    mqtterl_sessions:start_link(),
    {Session1, WasPresent1} = mqtterl_sessions:create(<<"cli1">>, #{}),
    {Session2, WasPresent2} = mqtterl_sessions:create(<<"cli1">>, #{}),
    ?assertEqual(false, WasPresent1),
    ?assertEqual(true, WasPresent2),
    ?assertNotEqual(Session1, Session2)
  after
    mqtterl_sessions:stop()
  end.

should_create_a_new_session_even_it_it_already_exists__test() ->
  try
    mqtterl_sessions:start_link(),
    {Session1, WasPresent1} = mqtterl_sessions:get_or_create(<<"cli1">>, #{}),
    {Session2, WasPresent2} = mqtterl_sessions:create(<<"cli1">>, #{}),
    ?assertEqual(false, WasPresent1),
    ?assertEqual(true, WasPresent2),
    ?assertNotEqual(Session1, Session2)
  after
    mqtterl_sessions:stop()
  end.