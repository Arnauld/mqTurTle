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
    {_Session, WasPresent} = mqtterl_sessions:get_or_create(<<"cli1">>, #{}),
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
    ?assertNotEqual(mqtterl_session:id(Session1), mqtterl_session:id(Session2))
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
    ?assertNotEqual(mqtterl_session:id(Session1), mqtterl_session:id(Session2))
  after
    mqtterl_sessions:stop()
  end.

should_register_subscriptions_and_return_those_matching_a_topic__test() ->
  try
    mqtterl_sessions:start_link(),
    ClientId1 = <<"cli1">>,
    ClientId2 = <<"cli2">>,
    ClientId3 = <<"cli3">>,
    mqtterl_sessions:create(ClientId1, #{}),
    mqtterl_sessions:create(ClientId2, #{}),
    mqtterl_sessions:create(ClientId3, #{}),
    mqtterl_sessions:add_subscriptions(ClientId1, [<<"sport/#">>, <<"cooking/lunch/">>]),
    mqtterl_sessions:add_subscriptions(ClientId2, [<<"sport/tennis">>]),
    mqtterl_sessions:add_subscriptions(ClientId3, [<<"alert/#">>]),
    %
    %
    Sessions0 = mqtterl_sessions:sessions_interested_by(<<"sport/tennis">>),
    ?assertEqual([ClientId1, ClientId2], to_id(Sessions0)),
    %
    Sessions1 = mqtterl_sessions:sessions_interested_by(<<"alert/crash">>),
    ?assertEqual([ClientId3], to_id(Sessions1))
  after
    mqtterl_sessions:stop()
  end.

to_id(Sessions) ->
  Ids = lists:map(fun({Id, _Session}) -> Id end, Sessions),
  lists:sort(Ids).