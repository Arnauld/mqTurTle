%%%-------------------------------------------------------------------
%%% @author Arnauld
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. mai 2016 19:48
%%%-------------------------------------------------------------------
-module(mqtterl_session).
-author("Arnauld").


-type topic_filter() :: binary().

-record(session, {
  id :: binary(),
  opts :: map(),
  subscriptions = [] :: [topic_filter()]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
  new/1,
  id/1,
  add_subscriptions/2,
  remove_subscriptions/2,
  is_interested_by/2]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new(Opts) ->
  #session{id = mqtterl_util:random_uuid(), opts = Opts}.

id(Session) when is_record(Session, session) ->
  Session#session.id.

is_interested_by(Session, Topic) when is_record(Session, session) ->
  has_match(Session#session.subscriptions, Topic).

add_subscriptions(Session, Filters) when is_record(Session, session) ->
  NewSubscriptions = Session#session.subscriptions ++ Filters,
  Session#session{subscriptions = NewSubscriptions}.

remove_subscriptions(Session, Filters) when is_record(Session, session) ->
  NewSubscriptions = Session#session.subscriptions -- Filters,
  Session#session{subscriptions = NewSubscriptions}.


has_match([], _) ->
  false;
has_match([Filter | Filters], Topic) ->
  case mqtterl_topic:match(Filter, Topic) of
    match ->
      true;
    _ ->
      has_match(Filters, Topic)
  end.